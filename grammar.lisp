;;; Helper Library and Random Seed
(load "util.in")

;;; Basic grammar, not recommended cause it's difficult to modify and maintain
(defun sentence ()
	(append (noun-phrase) (verb-phrase)))
(defun noun-phrase ()
	(append (Article) (Adj*) (Noun) (PP*)))
(defun verb-phrase ()
	(append (Verb) (noun-phrase)))
(defun Article ()
	(one-of '(the a)))
(defun Noun ()
	(one-of '(man woman horse cow)))
(defun Verb ()
	(one-of '(saw hit burp hiss)))
(defun Adj* ()
	(if (= (random 2) 0)
		nil
		(append (Adj) (Adj*))))
(defun PP* ()
	(if (random-elt '(t nil))
		(append (PP) (PP*))
		nil))
(defun PP ()
	(append (Prep) (noun-phrase)))
(defun Adj ()
	(one-of '(big little blue green)))
(defun Prep ()
	(one-of '(to in by with on)))

(defun one-of (the-list)
	"Pick a random element from the set and make it a list."
	(list (random-elt the-list)))

(defun random-elt (choices)
	"Choose a random element from the choices."
	(elt choices (random (length choices))))

(print (sentence))

;;; Much better way
;;; Use the most natural notation, then worry about writing an interpreter.
(defparameter *simple-grammar*
	'((sentence -> (noun-phrase verb-phrase))
		(noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
		(verb-phrase -> (Verb noun-phrase PP*))
		(PP* -> () (PP PP*))
		(Adj* -> () (Adj Adj*))
		(PP -> (Prep noun-phrase))
		(Prep -> to in by with on)
		(Adj -> big little blue green)
		(Article -> the a)
		(Noun -> man ball woman table)
		(Name -> Pat Kim Lee Terry)
		(Pronoun -> he she it these those)
		(Verb -> hit took saw liked))
	"A simple English grammar.")

(defvar *grammar* *simple-grammar*
	"The grammar used to generate.")

(defun rule-lhs (rule)
	"The left hand side of the rule."
	(first rule))

(defun rule-rhs (rule)
	"The right-hand side of a rule."
	(rest (rest rule)))

(defun rewrites (category)
	"Return a list of the possible rewrites for this category."
	(rule-rhs (assoc category *grammar*)))

(defun generate (phrase)
	"Generate a random sentence."
	(cond ((listp phrase)
				 (mappend #'generate phrase))
			  ((rewrites phrase)
			   (generate (random-elt (rewrites phrase))))
			  (t (list phrase))))
			   
(print (generate 'sentence))

(defun generate-tree (phrase)
	"Generate a random sentence or phrase,
	with a complete parse tree."
	(cond ((listp phrase)
				 (mapcar #'generate-tree phrase))
				((rewrites phrase)
				 (cons phrase
				 			 (generate-tree (random-elt (rewrites phrase)))))
				(t (list phrase))))

(print (generate-tree 'sentence))

(defun generate-all (phrase)
	"Generate a list of all possible expansions of this phrase."
	(cond ((null phrase) (list nil))
				((listp phrase)
				 (combine-all (generate-all (first phrase))
				 							(generate-all (rest phrase))))
				((rewrites phrase)
				 (mappend #'generate-all (rewrites phrase)))
				(t (list (list phrase)))))

(defun combine-all (xlist ylist)
	"Return a list of all lists formed by appending a y to an x."
	(mappend #'(lambda (y)
							 (mapcar #'(lambda (x) (append x y)) xlist))
					 ylist))

;;; (write (length (generate-all 'sentence)))
;;; Too big to hold in memory, needs a smaller language
