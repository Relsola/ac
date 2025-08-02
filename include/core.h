#pragma once

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "function.h"
#include "node.h"
#include "obj.h"
#include "token.h"

void error(char *fmt, ...);

void error_at(char *loc, char *fmt, ...);

void error_tok(Token *tok, char *fmt, ...);

Token *tokenize(char *p);

Function *parse(Token *tok);

void codegen(Function *prog);
