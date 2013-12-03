/*
 * unf_plugin.h
 *
 * Supplementary routines for unf plugins
 *
 * Copyright (c) 2006 Micah Altman
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Original copyright notice is retained at the end of this file.
 */

#include <getopt.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <locale.h>

// defaults
#define DEFAULTNDIGITS 7
#define DEFAULTCDIGITS 128
#define DEFAULTUNFVERSION 4.1

// implementation sizes
#define MAXBASE64LEN 60

typedef enum {STRING, INTEGER, REAL, UNF} unf_vectype;

typedef struct unf_s {
	double version;
	int ndigits;
	int cdigits;
	char base64[MAXBASE64LEN];
} unf_t;

/* compare strings through pointers */
int pstrcmp(const void*, const void*);
unf_t *parse_unf(const char* unf_string);
void unf_sort(char **unf_list, int unf_count);
