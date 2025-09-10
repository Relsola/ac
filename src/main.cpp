#include "core.h"

static bool opt_E;
static bool opt_S;
static bool opt_c;
static bool opt_cc1;
static bool opt_hash_hash_hash;
static char *opt_o;

char *base_file;
static char *output_file;

static std::vector<char *> input_paths;
static std::vector<char *> tmpfiles;

static void usage(int status) {
  fprintf(stderr, "ac [ -o <path> ] <file>\n");
  exit(status);
}

static bool take_arg(char *arg) { return !strcmp(arg, "-o"); }

static void parse_args(int argc, char **argv) {
  // Make sure that all command line options that take an argument
  // have an argument.
  for (int i = 1; i < argc; i++)
    if (take_arg(argv[i]))
      if (!argv[++i]) usage(1);

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

    if (!strcmp(argv[i], "-c")) {
      opt_c = true;
      continue;
    }

    if (!strcmp(argv[i], "-E")) {
      opt_E = true;
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

    if (argv[i][0] == '-' && argv[i][1] != '\0') error("unknown argument: %s", argv[i]);

    input_paths.push_back(argv[i]);
  }

  if (input_paths.size() == 0) error("no input files");
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

static void cc1(void) {
  // Tokenize and parse.
  Token *tok = tokenize_file(base_file);
  if (!tok) error("%s: %s", base_file, strerror(errno));

  tok = preprocess(tok);

  // If -E is given, print out preprocessed C code as a result.
  if (opt_E) {
    print_tokens(tok);
    return;
  }

  Obj *prog = parse(tok);

  // Traverse the AST to emit assembly.
  FILE *out = open_file(output_file);
  codegen(prog, out);
}

static void assemble(char *input, char *output) {
  char *cmd[] = {"as", "-c", input, "-o", output, NULL};
  run_subprocess(cmd);
}

static char *find_file(char *pattern) {
  char *path = NULL;
  glob_t buf = {};
  glob(pattern, 0, NULL, &buf);
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
  std::vector<char *> arr =
      {"ld", "-o", output, "-m", "elf_x86_64", "-dynamic-linker", "/lib64/ld-linux-x86-64.so.2"};

  char *libpath = find_libpath();
  char *gcc_libpath = find_gcc_libpath();

  arr.push_back(format("%s/crt1.o", libpath));
  arr.push_back(format("%s/crti.o", libpath));
  arr.push_back(format("%s/crtbegin.o", gcc_libpath));
  arr.push_back(format("-L%s", gcc_libpath));
  arr.push_back(format("-L%s", libpath));
  arr.push_back(format("-L%s/..", libpath));
  arr.push_back("-L/usr/lib64");
  arr.push_back("-L/lib64");
  arr.push_back("-L/usr/lib/x86_64-linux-gnu");
  arr.push_back("-L/usr/lib/x86_64-pc-linux-gnu");
  arr.push_back("-L/usr/lib/x86_64-redhat-linux");
  arr.push_back("-L/usr/lib");
  arr.push_back("-L/lib");

  for (auto &item : *inputs) arr.push_back(item);

  arr.push_back("-lc");
  arr.push_back("-lgcc");
  arr.push_back("--as-needed");
  arr.push_back("-lgcc_s");
  arr.push_back("--no-as-needed");
  arr.push_back(format("%s/crtend.o", gcc_libpath));
  arr.push_back(format("%s/crtn.o", libpath));
  arr.push_back(nullptr);

  run_subprocess(arr.data());
}

int main(int argc, char **argv) {
  atexit(cleanup);
  parse_args(argc, argv);

  if (opt_cc1) {
    cc1();
    return 0;
  }

  if (input_paths.size() > 1 && opt_o && (opt_c || opt_S | opt_E))
    error("cannot specify '-o' with '-c,' '-S' or '-E' with multiple files");

  std::vector<char *> ld_args = {};

  for (auto &input : input_paths) {
    char *output;
    if (opt_o)
      output = opt_o;
    else if (opt_S)
      output = replace_extn(input, ".s");
    else
      output = replace_extn(input, ".o");

    // Handle .o
    if (endswith(input, ".o")) {
      ld_args.push_back(input);
      continue;
    }

    // Handle .s
    if (endswith(input, ".s")) {
      if (!opt_S) assemble(input, output);
      continue;
    }

    // Handle .c
    if (!endswith(input, ".c") && strcmp(input, "-")) error("unknown file extension: %s", input);

    // Just preprocess
    if (opt_E) {
      run_cc1(argc, argv, input, NULL);
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
