#pragma once

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "token.h"

void error(char *fmt, ...);

void verror_at(char *loc, char *fmt, va_list ap);

void error_at(char *loc, char *fmt, ...);

void error_tok(Token *tok, char *fmt, ...);

Token *tokenize(char *p);
