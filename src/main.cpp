#include "core.hpp"

enum FileType {
  FILE_NONE,
  FILE_C,
  FILE_ASM,
  FILE_OBJ,
  FILE_AR,
  FILE_DSO,
};

std::vector<char *> include_paths;
bool opt_fcommon = true;
bool opt_fpic = false;

static FileType opt_x;
static std::vector<char *> opt_include;
static bool opt_E;
static bool opt_M;
static bool opt_MP;
static bool opt_MD;
static bool opt_MMD;
static bool opt_S;
static bool opt_c;
static bool opt_cc1;
static bool opt_hash_hash_hash;
static bool opt_static;
static char *opt_MF;
static char *opt_MT;
static char *opt_o;

static std::vector<char *> ld_extra_args;
static std::vector<char *> std_include_paths;

char *base_file;
static char *output_file;

static std::vector<char *> input_paths;
static std::vector<char *> tmpfiles;

static void usage(int status) {
  fprintf(stderr, "ac [ -o <path> ] <file>\n");
  exit(status);
}

static bool take_arg(char *arg) {
  char *x[] = {"-o", "-I", "-idirafter", "-include", "-x", "-MF", "-MT"};

  for (int i = 0; i < sizeof(x) / sizeof(*x); i++)
    if (!strcmp(arg, x[i])) return true;
  return false;
}

static void add_default_include_paths(char *argv0) {
  // We expect that chibicc-specific include files are installed
  // to ./include relative to argv[0].
  include_paths.push_back(format("%s/include", dirname(strdup(argv0))));

  // Add standard include paths.
  include_paths.push_back("/usr/local/include");
  include_paths.push_back("/usr/include/x86_64-linux-gnu");
  include_paths.push_back("/usr/include");

  // Keep a copy of the standard include paths for -MMD option.
  for (auto &path : include_paths) std_include_paths.push_back(path);
}

static FileType parse_opt_x(char *s) {
  if (!strcmp(s, "c")) return FILE_C;
  if (!strcmp(s, "assembler")) return FILE_ASM;
  if (!strcmp(s, "none")) return FILE_NONE;
  error("<command line>: unknown argument for -x: %s", s);
}

static void define(char *str) {
  char *eq = strchr(str, '=');
  if (eq)
    define_macro(strndup(str, eq - str), eq + 1);
  else
    define_macro(str, "1");
}

static char *quote_makefile(char *s) {
  char *buf = new char[strlen(s) * 2 + 1]();

  for (int i = 0, j = 0; s[i]; i++) {
    switch (s[i]) {
      case '$':
        buf[j++] = '$';
        buf[j++] = '$';
        break;
      case '#':
        buf[j++] = '\\';
        buf[j++] = '#';
        break;
      case ' ':
      case '\t':
        for (int k = i - 1; k >= 0 && s[k] == '\\'; k--) buf[j++] = '\\';
        buf[j++] = '\\';
        buf[j++] = s[i];
        break;
      default:
        buf[j++] = s[i];
        break;
    }
  }
  return buf;
}

static void parse_args(int argc, char **argv) {
  // Make sure that all command line options that take an argument
  // have an argument.
  for (int i = 1; i < argc; i++)
    if (take_arg(argv[i]))
      if (!argv[++i]) usage(1);

  std::vector<char *> idirafter;

  for (int i = 1; i < argc; i++) {
    if (!strcmp(argv[i], "-###")) {
      opt_hash_hash_hash = true;
      continue;
    }

    if (!strcmp(argv[i], "-cc1")) {
      opt_cc1 = true;
      continue;
    }

    if (!strcmp(argv[i], "--help")) usage(0);

    if (!strcmp(argv[i], "-o")) {
      opt_o = argv[++i];
      continue;
    }

    if (!strncmp(argv[i], "-o", 2)) {
      opt_o = argv[i] + 2;
      continue;
    }

    if (!strcmp(argv[i], "-S")) {
      opt_S = true;
      continue;
    }

    if (!strcmp(argv[i], "-fcommon")) {
      opt_fcommon = true;
      continue;
    }

    if (!strcmp(argv[i], "-fno-common")) {
      opt_fcommon = false;
      continue;
    }

    if (!strcmp(argv[i], "-c")) {
      opt_c = true;
      continue;
    }

    if (!strcmp(argv[i], "-E")) {
      opt_E = true;
      continue;
    }

    if (!strncmp(argv[i], "-I", 2)) {
      include_paths.push_back(argv[i] + 2);
      continue;
    }

    if (!strcmp(argv[i], "-D")) {
      define(argv[++i]);
      continue;
    }

    if (!strncmp(argv[i], "-D", 2)) {
      define(argv[i] + 2);
      continue;
    }

    if (!strcmp(argv[i], "-U")) {
      undef_macro(argv[++i]);
      continue;
    }

    if (!strcmp(argv[i], "-include")) {
      opt_include.push_back(argv[++i]);
      continue;
    }

    if (!strcmp(argv[i], "-x")) {
      opt_x = parse_opt_x(argv[++i]);
      continue;
    }

    if (!strncmp(argv[i], "-x", 2)) {
      opt_x = parse_opt_x(argv[i] + 2);
      continue;
    }

    if (!strncmp(argv[i], "-l", 2)) {
      include_paths.push_back(argv[i]);
      continue;
    }

    if (!strcmp(argv[i], "-s")) {
      ld_extra_args.push_back("-s");
      continue;
    }

    if (!strcmp(argv[i], "-M")) {
      opt_M = true;
      continue;
    }

    if (!strcmp(argv[i], "-MF")) {
      opt_MF = argv[++i];
      continue;
    }

    if (!strcmp(argv[i], "-MP")) {
      opt_MP = true;
      continue;
    }

    if (!strcmp(argv[i], "-MT")) {
      if (opt_MT == nullptr)
        opt_MT = argv[++i];
      else
        opt_MT = format("%s %s", opt_MT, argv[++i]);
      continue;
    }

    if (!strcmp(argv[i], "-MD")) {
      opt_MD = true;
      continue;
    }

    if (!strcmp(argv[i], "-MQ")) {
      if (opt_MT == nullptr)
        opt_MT = quote_makefile(argv[++i]);
      else
        opt_MT = format("%s %s", opt_MT, quote_makefile(argv[++i]));
      continue;
    }

    if (!strcmp(argv[i], "-MMD")) {
      opt_MD = opt_MMD = true;
      continue;
    }

    if (!strcmp(argv[i], "-fpic") || !strcmp(argv[i], "-fPIC")) {
      opt_fpic = true;
      continue;
    }

    if (!strncmp(argv[i], "-U", 2)) {
      undef_macro(argv[i] + 2);
      continue;
    }

    if (!strcmp(argv[i], "-cc1-input")) {
      base_file = argv[++i];
      continue;
    }

    if (!strcmp(argv[i], "-cc1-output")) {
      output_file = argv[++i];
      continue;
    }

    if (!strcmp(argv[i], "-idirafter")) {
      idirafter.push_back(argv[i++]);
      continue;
    }

    if (!strcmp(argv[i], "-static")) {
      opt_static = true;
      ld_extra_args.push_back("-static");
      continue;
    }

    // These options are ignored for now.
    if (!strncmp(argv[i], "-O", 2) || !strncmp(argv[i], "-W", 2) || !strncmp(argv[i], "-g", 2) ||
        !strncmp(argv[i], "-std=", 5) || !strcmp(argv[i], "-ffreestanding") ||
        !strcmp(argv[i], "-fno-builtin") || !strcmp(argv[i], "-fno-omit-frame-pointer") ||
        !strcmp(argv[i], "-fno-stack-protector") || !strcmp(argv[i], "-fno-strict-aliasing") ||
        !strcmp(argv[i], "-m64") || !strcmp(argv[i], "-mno-red-zone") || !strcmp(argv[i], "-w"))
      continue;

    if (argv[i][0] == '-' && argv[i][1] != '\0') error("unknown argument: %s", argv[i]);

    input_paths.push_back(argv[i]);
  }

  for (auto &path : idirafter) include_paths.push_back(path);

  if (input_paths.size() == 0) error("no input files");

  // -E implies that the input is the C macro language.
  if (opt_E) opt_x = FILE_C;
}

static FILE *open_file(char *path) {
  if (!path || strcmp(path, "-") == 0) return stdout;

  FILE *out = fopen(path, "w");
  if (!out) error("cannot open output file: %s: %s", path, strerror(errno));
  return out;
}

static bool endswith(char *p, char *q) {
  int len1 = strlen(p);
  int len2 = strlen(q);
  return (len1 >= len2) && !strcmp(p + len1 - len2, q);
}

// Replace file extension
static char *replace_extn(char *tmpl, char *extn) {
  char *filename = basename(strdup(tmpl));
  char *dot = strrchr(filename, '.');
  if (dot) *dot = '\0';
  return format("%s%s", filename, extn);
}

static void cleanup(void) {
  for (auto &item : tmpfiles) unlink(item);
}

static char *create_tmpfile(void) {
  char *path = strdup("/tmp/ac-XXXXXX");
  int fd = mkstemp(path);
  if (fd == -1) error("mkstemp failed: %s", strerror(errno));
  close(fd);
  tmpfiles.push_back(path);
  return path;
}

static void run_subprocess(char **argv) {
  // If -### is given, dump the subprocess's command line.
  if (opt_hash_hash_hash) {
    fprintf(stderr, "%s", argv[0]);
    for (int i = 1; argv[i]; i++) fprintf(stderr, " %s", argv[i]);
    fprintf(stderr, "\n");
  }

  if (fork() == 0) {
    // Child process. Run a new command.
    execvp(argv[0], argv);
    fprintf(stderr, "exec failed: %s: %s\n", argv[0], strerror(errno));
    _exit(1);
  }

  // Wait for the child process to finish.
  int status;
  while (wait(&status) > 0)
    ;
  if (status != 0) exit(1);
}

static void run_cc1(int argc, char **argv, char *input, char *output) {
  char **args = new char *[argc + 10]();
  memcpy(args, argv, argc * sizeof(char *));
  args[argc++] = "-cc1";

  if (input) {
    args[argc++] = "-cc1-input";
    args[argc++] = input;
  }

  if (output) {
    args[argc++] = "-cc1-output";
    args[argc++] = output;
  }

  run_subprocess(args);
}

// Print tokens to stdout. Used for -E.
static void print_tokens(Token *tok) {
  FILE *out = open_file(opt_o ? opt_o : (char *)"-");

  int line = 1;
  for (; tok->kind != TokenKind::TK_EOF; tok = tok->next) {
    if (line > 1 && tok->at_bol) fprintf(out, "\n");
    if (tok->has_space && !tok->at_bol) fprintf(out, " ");
    fprintf(out, "%.*s", tok->len, tok->loc);
    line++;
  }
  fprintf(out, "\n");
}

static bool in_std_include_path(char *path) {
  for (auto &dir : std_include_paths) {
    int len = strlen(dir);
    if (strncmp(dir, path, len) == 0 && path[len] == '/') return true;
  }
  return false;
}

// If -M options is given, the compiler write a list of input files to
// stdout in a format that "make" command can read. This feature is
// used to automate file dependency management.
static void print_dependencies() {
  char *path;
  if (opt_MF)
    path = opt_MF;
  else if (opt_MD)
    path = replace_extn(opt_o ? opt_o : base_file, ".d");
  else if (opt_o)
    path = opt_o;
  else
    path = "-";

  FILE *out = open_file(path);
  if (opt_MT)
    fprintf(out, "%s:", opt_MT);
  else
    fprintf(out, "%s:", quote_makefile(replace_extn(base_file, ".o")));

  std::vector<File *> files = get_input_files();

  for (auto &file : files) {
    if (opt_MMD && in_std_include_path(file->name)) continue;
    fprintf(out, " \\\n  %s", file->name);
  }
  fprintf(out, "\n\n");

  if (opt_MP)
    for (auto &file : files) {
      if (opt_MMD && in_std_include_path(file->name)) continue;
      fprintf(out, "%s:\n\n", quote_makefile(file->name));
    }
}

static Token *must_tokenize_file(char *path) {
  Token *tok = tokenize_file(path);
  if (!tok) error("%s: %s", path, strerror(errno));
  return tok;
}

static Token *append_tokens(Token *tok1, Token *tok2) {
  if (!tok1 || tok1->kind == TokenKind::TK_EOF) return tok2;

  Token *t = tok1;
  while (t->next->kind != TokenKind::TK_EOF) t = t->next;
  t->next = tok2;
  return tok1;
}

static void cc1(void) {
  Token *tok = nullptr;

  // Process -include option
  for (auto &incl : opt_include) {
    char *path;
    if (file_exists(incl)) {
      path = incl;
    } else {
      path = search_include_paths(incl);
      if (!path) error("-include: %s: %s", incl, strerror(errno));
    }

    Token *tok2 = must_tokenize_file(path);
    tok = append_tokens(tok, tok2);
  }

  // Tokenize and parse.
  Token *tok2 = must_tokenize_file(base_file);
  tok = append_tokens(tok, tok2);
  tok = preprocess(tok);

  // If -M or -MD are given, print file dependencies.
  if (opt_M || opt_MD) {
    print_dependencies();
    if (opt_M) return;
  }

  // If -E is given, print out preprocessed C code as a result.
  if (opt_E) {
    print_tokens(tok);
    return;
  }

  Obj *prog = parse(tok);

  // Open a temporary output buffer.
  char *buf;
  size_t buflen;
  FILE *output_buf = open_memstream(&buf, &buflen);

  // Traverse the AST to emit assembly.
  codegen(prog, output_buf);
  fclose(output_buf);

  // Write the asembly text to a file.
  FILE *out = open_file(output_file);
  fwrite(buf, buflen, 1, out);
  fclose(out);
}

static void assemble(char *input, char *output) {
  char *cmd[] = {"as", "-c", input, "-o", output, nullptr};
  run_subprocess(cmd);
}

static char *find_file(char *pattern) {
  char *path = nullptr;
  glob_t buf = {};
  glob(pattern, 0, nullptr, &buf);
  if (buf.gl_pathc > 0) path = strdup(buf.gl_pathv[buf.gl_pathc - 1]);
  globfree(&buf);
  return path;
}

// Returns true if a given file exists.
bool file_exists(char *path) {
  struct stat st;
  return !stat(path, &st);
}

static char *find_libpath(void) {
  if (file_exists("/usr/lib/x86_64-linux-gnu/crti.o")) return "/usr/lib/x86_64-linux-gnu";
  if (file_exists("/usr/lib64/crti.o")) return "/usr/lib64";
  error("library path is not found");
}

static char *find_gcc_libpath(void) {
  char *paths[] = {
      "/usr/lib/gcc/x86_64-linux-gnu/*/crtbegin.o",
      "/usr/lib/gcc/x86_64-pc-linux-gnu/*/crtbegin.o",  // For Gentoo
      "/usr/lib/gcc/x86_64-redhat-linux/*/crtbegin.o",  // For Fedora
  };

  for (int i = 0; i < sizeof(paths) / sizeof(*paths); i++) {
    char *path = find_file(paths[i]);
    if (path) return dirname(path);
  }

  error("gcc library path is not found");
}

static void run_linker(std::vector<char *> *inputs, char *output) {
  std::vector<char *> arr = {"ld", "-o", output, "-m", "elf_x86_64"};

  char *libpath = find_libpath();
  char *gcc_libpath = find_gcc_libpath();

  arr.push_back(format("%s/crt1.o", libpath));
  arr.push_back(format("%s/crti.o", libpath));
  arr.push_back(format("%s/crtbegin.o", gcc_libpath));
  arr.push_back(format("-L%s", gcc_libpath));
  arr.push_back("-L/usr/lib64");
  arr.push_back("-L/lib64");
  arr.push_back("-L/usr/lib/x86_64-linux-gnu");
  arr.push_back("-L/usr/lib/x86_64-pc-linux-gnu");
  arr.push_back("-L/usr/lib/x86_64-redhat-linux");
  arr.push_back("-L/usr/lib");
  arr.push_back("-L/lib");

  if (!opt_static) {
    arr.push_back("-dynamic-linker");
    arr.push_back("/lib64/ld-linux-x86-64.so.2");
  }

  for (auto &item : ld_extra_args) arr.push_back(item);

  for (auto &item : *inputs) arr.push_back(item);

  if (opt_static) {
    arr.push_back("--start-group");
    arr.push_back("-lgcc");
    arr.push_back("-lgcc_eh");
    arr.push_back("-lc");
    arr.push_back("--end-group");
  } else {
    arr.push_back("-lc");
    arr.push_back("-lgcc");
    arr.push_back("--as-needed");
    arr.push_back("-lgcc_s");
    arr.push_back("--no-as-needed");
  }

  arr.push_back(format("%s/crtend.o", gcc_libpath));
  arr.push_back(format("%s/crtn.o", libpath));
  arr.push_back(nullptr);

  run_subprocess(arr.data());
}

static FileType get_file_type(char *filename) {
  if (opt_x != FILE_NONE) return opt_x;

  if (endswith(filename, ".a")) return FILE_AR;
  if (endswith(filename, ".so")) return FILE_DSO;
  if (endswith(filename, ".o")) return FILE_OBJ;
  if (endswith(filename, ".c")) return FILE_C;
  if (endswith(filename, ".s")) return FILE_ASM;

  error("<command line>: unknown file extension: %s", filename);
}

int main(int argc, char **argv) {
  atexit(cleanup);
  init_macros();
  parse_args(argc, argv);

  if (opt_cc1) {
    add_default_include_paths(argv[0]);
    cc1();
    return 0;
  }

  if (input_paths.size() > 1 && opt_o && (opt_c || opt_S | opt_E))
    error("cannot specify '-o' with '-c,' '-S' or '-E' with multiple files");

  std::vector<char *> ld_args = {};

  for (auto &input : input_paths) {
    if (!strncmp(input, "-l", 2)) {
      ld_args.push_back(input);
      continue;
    }

    char *output;
    if (opt_o)
      output = opt_o;
    else if (opt_S)
      output = replace_extn(input, ".s");
    else
      output = replace_extn(input, ".o");

    FileType type = get_file_type(input);

    // Handle .o or .a
    if (type == FILE_OBJ || type == FILE_AR || type == FILE_DSO) {
      ld_args.push_back(input);
      continue;
    }

    // Handle .s
    if (type == FILE_ASM) {
      if (!opt_S) assemble(input, output);
      continue;
    }

    // Just preprocess
    if (opt_E || opt_M) {
      run_cc1(argc, argv, input, nullptr);
      continue;
    }

    // Compile
    if (opt_S) {
      run_cc1(argc, argv, input, output);
      continue;
    }

    // Compile and assemble
    if (opt_c) {
      char *tmp = create_tmpfile();
      run_cc1(argc, argv, input, tmp);
      assemble(tmp, output);
      continue;
    }

    // Compile, assemble and link
    char *tmp1 = create_tmpfile();
    char *tmp2 = create_tmpfile();
    run_cc1(argc, argv, input, tmp1);
    assemble(tmp1, tmp2);
    ld_args.push_back(tmp2);
    continue;
  }

  if (ld_args.size() > 0) run_linker(&ld_args, opt_o ? opt_o : (char *)"a.out");
  return 0;
}
