#ifndef LIBSISP_H

#define BUILTIN_SIZE sizeof(builtin_functions) / sizeof(char)

static char builtin_functions[] =
	"(define u {tau : (equal tau tau)})"
	"(define (caar x) (car (car x)))"
	"(define (cadr x) (car (cdr x)))"
	"(define (cdar x) (cdr (car x)))"
	"(define (cddr x) (cdr (cdr x)))"
	"(define (caaar x) (car (car (car x))))"
	"(define (caadr x) (car (car (cdr x))))"
	"(define (cadar x) (car (cdr (car x))))"
	"(define (caddr x) (car (cdr (cdr x))))"
	"(define (cdaar x) (cdr (car (car x))))"
	"(define (cdadr x) (cdr (car (cdr x))))"
	"(define (cddar x) (cdr (cdr (car x))))"
	"(define (cdddr x) (cdr (cdr (cdr x))))"
	"(define (butlast l)"
	"	(cond ((eq (cdr l) nil) nil)"
	"	(t (cons (car l) (butlast (cdr l))))))"
	"(define (get-names x)"
	"	(cond ((eq x nil) nil)"
	"	(t (cons (caar x) (get-names (cdr x))))))"
	"(define (get-values x)"
	"	(cond ((eq x nil) nil)"
	"	(t (cons (cadar x) (get-values (cdr x))))))"
	"(define (unpair x) (list (get-names x) (get-values x)))"
	"(define (succ x y)"
	"	(cond ((or (eq y nil) (eq nil (cdr y))) nil)"
	"	((eq (car y) x) (cadr y))"
	"	(t (succ x (cdr y)))))"
	"(define (pred x y)"
	"	(cond ((or (eq y nil) (eq nil (cdr y))) nil)"
	"	((eq (cadr y) x) (car y))"
	"	(t (pred x (cdr y)))))";

#define LIBSISP_H
#endif
