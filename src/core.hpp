#pragma once

#include <ctype.h>
#include <errno.h>
#include <glob.h>
#include <libgen.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#include <algorithm>
#include <functional>
#include <string>
#include <vector>

class Type;
class Node;
class Member;
class Relocation;
struct Hideset;

//
// strings.c
//
char *format(char *fmt, ...);

enum class TokenKind : int {
  TK_IDENT,    // Identifiers
  TK_PUNCT,    // Punctuators
  TK_KEYWORD,  // Keywords
  TK_STR,      // String literals
  TK_NUM,      // Numeric literals
  TK_PP_NUM,   // Preprocessing numbers
  TK_EOF,      // End-of-file markers
};

// AST node
enum class NodeKind : int {
  INVALID,
  ND_NULL_EXPR,  // Do nothing
  ND_ADD,        // +
  ND_SUB,        // -
  ND_MUL,        // *
  ND_DIV,        // /
  ND_NEG,        // unary -
  ND_MOD,        // %
  ND_BITAND,     // &
  ND_BITOR,      // |
  ND_BITXOR,     // ^
  ND_SHL,        // <<
  ND_SHR,        // >>
  ND_EQ,         // ==
  ND_NE,         // !=
  ND_LT,         // <
  ND_LE,         // <=
  ND_ASSIGN,     // =
  ND_COND,       // ?:
  ND_COMMA,      // ,
  ND_MEMBER,     // . (struct member access)
  ND_ADDR,       // unary &
  ND_DEREF,      // unary *
  ND_NOT,        // !
  ND_BITNOT,     // ~
  ND_LOGAND,     // &&
  ND_LOGOR,      // ||
  ND_RETURN,     // "return"
  ND_IF,         // "if"
  ND_FOR,        // "for" or "while"
  ND_DO,         // "do"
  ND_SWITCH,     // "switch"
  ND_CASE,       // "case"
  ND_BLOCK,      // { ... }
  ND_GOTO,       // "goto"
  ND_LABEL,      // Labeled statement
  ND_FUNCALL,    // Function call
  ND_EXPR_STMT,  // Expression statement
  ND_STMT_EXPR,  // Statement expression
  ND_VAR,        // Variable
  ND_NUM,        // Integer
  ND_CAST,       // Type cast
  ND_MEMZERO,    // Zero-clear a stack variable
};

enum class TypeKind : int {
  INVALID,
  TY_VOID,
  TY_CHAR,
  TY_BOOL,
  TY_SHORT,
  TY_INT,
  TY_LONG,
  TY_FLOAT,
  TY_DOUBLE,
  TY_ENUM,
  TY_PTR,
  TY_FUNC,
  TY_ARRAY,
  TY_STRUCT,
  TY_UNION,
};

struct File {
  char *name = nullptr;
  int file_no = 0;
  char *contents = nullptr;

  // For #line directive
  char *display_name = nullptr;
  int line_delta = 0;
};

class Token {
 public:
  TokenKind kind = TokenKind::TK_EOF;  // Token kind
  Token *next = nullptr;               // Next token
  int64_t val = 0;                     // If kind is TK_NUM, its value
  double fval = 0;                     // If kind is TK_NUM, its value
  char *loc = nullptr;                 // Token location
  int len = 0;                         // Token length
  Type *ty = nullptr;                  // Used if TK_NUM or TK_STR
  char *str = nullptr;                 // String literal contents including terminating '\0'

  File *file = nullptr;        // Source location
  char *filename = nullptr;    // Filename
  int line_no = 0;             // Line number
  int line_delta = 0;          // Line number
  bool at_bol = false;         // True if this token is at beginning of line
  bool has_space = false;      // True if this token follows a space character
  Hideset *hideset = nullptr;  // For macro expansion
  Token *origin = nullptr;     // If this is expanded from a macro, the original token

  Token() = default;

  Token(TokenKind kind, char *start, char *end) : kind(kind), loc(start), len(end - start){};

  bool equal(char *op) { return memcmp(this->loc, op, this->len) == 0 && op[this->len] == '\0'; };

  Token *skip(char *s);

  long get_number();

  bool consume(Token **rest, char *str);
};

// Variable or function
class Obj {
 public:
  Obj *next = nullptr;
  char *name = nullptr;   // Variable name
  Type *ty = nullptr;     // Type
  Token *tok = nullptr;   // representative token
  bool is_local = false;  // local or global/function
  int align = 0;          // alignment

  // Local variable
  int offset = 0;

  // Global variable or function
  bool is_function = false;
  bool is_definition = false;
  bool is_static = false;

  // Global variable
  char *init_data = nullptr;
  Relocation *rel = nullptr;

  // Function
  Obj *params = nullptr;
  Node *body = nullptr;
  Obj *locals = nullptr;
  Obj *va_area = nullptr;
  int stack_size = 0;

  Obj() = default;

  Obj(char *name, Type *ty) : name(name), ty(ty){};
};

// Global variable can be initialized either by a constant expression
// or a pointer to another global variable. This struct represents the
// latter.
class Relocation {
 public:
  Relocation *next = nullptr;
  int offset = 0;
  char *label = nullptr;
  long addend = 0;

  Relocation() = default;
};

class Node {
 public:
  NodeKind kind = NodeKind::INVALID;  // Node kind
  Node *next = nullptr;               // Next node
  Type *ty = nullptr;                 // Type, e.g. int or pointer to int
  Token *tok = nullptr;               // Representative token

  Node *lhs = nullptr;  // Left-hand side
  Node *rhs = nullptr;  // Right-hand side

  // "if" or "for" statement
  Node *cond = nullptr;
  Node *then = nullptr;
  Node *els = nullptr;
  Node *init = nullptr;
  Node *inc = nullptr;

  // "break" and "continue" labels
  char *brk_label = nullptr;
  char *cont_label = nullptr;

  // Block or statement expression
  Node *body = nullptr;

  // Struct member access
  Member *member = nullptr;

  // Function call
  Type *func_ty = nullptr;
  Node *args = nullptr;
  bool pass_by_stack = false;
  Obj *ret_buffer = nullptr;

  // Goto or labeled statement
  char *label = nullptr;
  char *unique_label = nullptr;
  Node *goto_next = nullptr;

  // Switch-cases
  Node *case_next = nullptr;
  Node *default_case = nullptr;

  // Variable
  Obj *var = nullptr;

  // Numeric literal
  int64_t val = 0;
  double fval = 0;

  Node() = default;
};

class Type {
 public:
  static Type *ty_void;
  static Type *ty_bool;

  static Type *ty_char;
  static Type *ty_short;
  static Type *ty_int;
  static Type *ty_long;

  static Type *ty_uchar;
  static Type *ty_ushort;
  static Type *ty_uint;
  static Type *ty_ulong;

  static Type *ty_float;
  static Type *ty_double;

  static Type *pointer_to(Type *base);

  static Type *func_type(Type *return_ty);

  static Type *copy_type(Type *ty);

  static Type *array_of(Type *base, int size);

  static Type *enum_type();

  static Type *struct_type();

  static bool is_compatible(Type *t1, Type *t2);

  TypeKind kind = TypeKind::INVALID;

  int size = 0;              // sizeof() value
  int align = 0;             // alignment
  bool is_unsigned = false;  // unsigned or signed
  Type *origin = nullptr;    // for type compatibility check

  // Pointer-to or array-of type. We intentionally use the same member
  // to represent pointer/array duality in C.
  //
  // In many contexts in which a pointer is expected, we examine this
  // member instead of "kind" member to determine whether a type is a
  // pointer or not. That means in many contexts "array of T" is
  // naturally handled as if it were "pointer to T", as required by
  // the C spec.
  Type *base = nullptr;

  // Declaration
  Token *name = nullptr;
  Token *name_pos = nullptr;

  // Array
  int array_len = 0;

  // Struct
  Member *members = nullptr;
  bool is_flexible = false;

  // Function type
  Type *return_ty = nullptr;
  Type *params = nullptr;
  bool is_variadic = false;
  Type *next = nullptr;

  Type() = default;

  Type(TypeKind kind) : kind(kind){};

  Type(TypeKind kind, int size, int align) : kind(kind), size(size), align(align){};

  Type(TypeKind kind, int size, int align, bool is_unsigned)
      : kind(kind), size(size), align(align), is_unsigned(is_unsigned){};

  bool is_integer() {
    TypeKind k = this->kind;
    return k == TypeKind::TY_BOOL || k == TypeKind::TY_CHAR || k == TypeKind::TY_SHORT ||
           k == TypeKind::TY_INT || k == TypeKind::TY_LONG || k == TypeKind::TY_ENUM;
  };

  bool is_flonum() {
    return this->kind == TypeKind::TY_FLOAT || this->kind == TypeKind::TY_DOUBLE;
  };

  bool is_numeric() { return this->is_integer() || this->is_flonum(); };
};

class Member {
 public:
  Member *next = nullptr;
  Type *ty = nullptr;
  Token *tok = nullptr;  // for error message
  Token *name = nullptr;
  int idx = 0;
  int align = 0;
  int offset = 0;

  // Bitfield
  bool is_bitfield = false;
  int bit_offset = 0;
  int bit_width = 0;

  Member() = default;
};

Node *new_cast(Node *expr, Type *ty);

int64_t const_expr(Token **rest, Token *tok);

void add_type(Node *node);

[[noreturn]] void error(char *fmt, ...);

[[noreturn]] void error_at(char *loc, char *fmt, ...);

[[noreturn]] void error_tok(Token *tok, char *fmt, ...);

void warn_tok(Token *tok, char *fmt, ...);

void convert_pp_tokens(Token *tok);

std::vector<File *> get_input_files();

File *new_file(char *name, int file_no, char *contents);

Token *tokenize_string_literal(Token *tok, Type *basety);

Token *tokenize(File *file);

Token *tokenize_file(char *filename);

Obj *parse(Token *tok);

int align_to(int n, int align);

void codegen(Obj *prog, FILE *out);

#define unreachable() error("internal error at %s:%d", __FILE__, __LINE__)

//
// preprocess.c
//

void init_macros(void);
void define_macro(char *name, char *buf);
void undef_macro(char *name);
Token *preprocess(Token *tok);

//
// unicode.c
//

int encode_utf8(char *buf, uint32_t c);
uint32_t decode_utf8(char **new_pos, char *p);
bool is_ident1(uint32_t c);
bool is_ident2(uint32_t c);
int display_width(char *p, int len);

//
// main.c
//

bool file_exists(char *path);

extern std::vector<char *> include_paths;
extern char *base_file;
