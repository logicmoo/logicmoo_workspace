;;; -*- Package: DOMAINS; Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :domains)

;;; Example used by Paul Morris in AAAI paper to illustrate loop control
;;; problems in planning.  robot1 is solvable, robot2 has no solution.

(define (domain morris)
  (:requirements :strips :equality)
  (:predicates (charged) (at ?l) (hole))
  (:constants B)
  (:Action GoTo
	     :parameters (?to ?from)
	     :precondition (and (charged) (at ?from) (not (= ?from ?to)))
	     :effect (and (at ?to) (not (at ?from))))

  (:Action Fixhole
	     :parameters ()
	     :precondition (and (charged) (hole))
	     :effect (and (not (charged)) (not (hole))))

  (:Action Recharge
	     :parameters ()
	     :precondition (and (at B) (not (charged)))
	     :effect (charged)))

(define (problem morris-robot1)
    (:domain morris)
  (:objects A)
  (:goal (and (charged) (not (hole))))
  (:init (at A) (charged) (hole))
  )

(define (problem morris-robot2)
    (:domain morris)
  (:objects A)
  (:goal (and (charged) (not (hole))))
  (:init (at A) (not (charged)) (hole))
  )

