	/* -*- mode: asm; fill-column: 88; tab-width: 8; -*- */

	/*

	Get Started with Arpilisp

	Side note: if this file looks funny, use a text editor that displays a
	fixed-width font and uses 8 spaces for tabs.

	Arpilisp is the Assembled Raspberry Pi Lisp.  This tutorial uses assembly
	language, the lowest level programming language, to implement an interpreter for
	Lisp, the highest level language.  Arpilisp implements Lisp from scratch, with
	no help from any libraries and minimal help from the kernel.

	Before you use arpilisp, you need to build it.  To build arpilisp, you need
	a few things:

	* A Raspberry Pi running Raspbian

	* That's it.

	To build arpilisp, type the following in a shell:

	gcc -nostdlib -o arpilisp arpilisp.s

	This tells the GNU compiler (gcc) to use arpilisp.s, this file that you are
	reading, as input, to build an executable file named arpilisp.  Later we
	cover what the -nostdlib option does.

	To use arpilisp itself, type this:

	./arpilisp

	To quit arpilisp, type Ctrl-D.

	References

	https://www.raspberrypi.org

	https://www.raspbian.org
	
	________________________________________________________________________________

	Acknowledgements

	Thanks to Richard W.M. Jones, John McCarthy, and Jack W. Crenshaw for showing us
	the simplicity and elegance hiding in complex things, and for reminding us that
	computers are awesome.

	Thanks also to Chris Hinsley and Jay Sissom for feedback.

	________________________________________________________________________________

	The Shortest Introduction to Lisp

	Lisp is one of the oldest programming languages in computing.  John McCarthy and
	his team implemented the first Lisp interpreter in 1960.  They wanted a
	programming language for AI research.  Since then, he and many others discovered
	amazing things about how Lisp can make computers do surprising, interesting
	things.

	Most people acknowledge Lisp's influence without knowing what it is exactly.
	Others are turned off by Lisp's weirdness.  Lisp continues to influence
	programming languages today.  It's not as inaccessible, alien, or irrelevant as
	you might believe.

	Lisp is an acronym for LISt Processor.  A list is a sequence of items.  To
	specify a list, we start with an opening parenthesis, continue with the things
	in the list, and end with a closing parenthesis.

	For example, here's a list of ingredients for a salad:

	(lettuce tomato oil vinegar)

	We can use extra spaces to clarify what we type but Lisp doesn't care about
	excessive white space between list items, as long as there is some.  And there's
	no need to put white space around parentheses.  Here are some lists that look
	different to us but mean the same to Lisp:

	(lettuce  tomato   oil	  vinegar     )

	(lettuce
	tomato
	oil
	vinegar)

	A list may itself also contain lists.  Here's a list that is different from our
	previous list:

	((lettuce tomato) (oil vinegar))

	While it contains the same items, they are arranged into two sub-lists, the
	first for the vegetables (lettuce tomato), the second for the dressing (oil
	vinegar).

	Here's an empty list:

	()

	The empty list comes up so much that Lisp has a symbol for it:

	nil

	A symbol is just a name that represents something.  What the symbol represents
	depends on you, the programmer.  In our list above, lettuce, tomato, oil, and
	vinegar are symbols for salad ingredients.  We sometimes refer to symbols as
	atoms.  Unlike lists, atoms are not composed of parts.

	In Lisp, lists and atoms are called "S-expressions".  We also use just
	"expression" to mean the same thing.

	The Processor part of LISt Processor means that we program a Lisp
	interpreter to manipulate lists.  We manipulate lists with functions that accept
	S-expression arguments.

	Here's a cool thing about Lisp: functions are also written as S-expressions.  In
	other words, data and programs have the same form.

	Side note: A lot of people make a big deal about the interchangeability of Lisp
	data and programs.  That's certainly remarkable and accounts for a lot of Lisp's
	elegance and expressiveness.  The irony is that after getting used to this idea,
	you find it odd that other programming languages don't have this feature.

	To see how functions work, let's take a closer look at nil and the empty list.
	To verify that they are the same, we enter this expression in Lisp:

	(eq nil () )

	The first item in the list of our expression is eq, which is a function.  A
	function tells Lisp what needs to be done: we want to determine equality.  The
	remaining items in the list, nil and (), specify the data that we want the eq
	function to process.

	Lisp responds with:

	t

	The t symobl means that the result of this expression is true; nil and () are
	indeed equal. Using the t symbol to represent "true" is a Lisp convention, while
	nil is the conventional value for false.

	In Lisp, we say "function application" to mean applying a function to arguments.
	In our example, we apply eq to two arguments.  The function compares them for
	equality, then returns a "t" for true or "nil" for false.

	Before Lisp applies a function to its arguments, it first evaluates each
	argument.  For example, let's take a look at a more complicated expression:

	(eq
	  (eq nil ())
	  (eq () nil))

	In this expression, we give eq these arguments:

	(eq nil ())

	and

	(eq () nil)
	
	We can't determine how to compute the value of the first eq application until we
	know the values of its arguments.  Behind the scenes, that's what Lisp does.  If
	we could watch the internal computation of our S-expression, we would see Lisp
	first compute then substitute the values of the arguments.

	This substitution renders our expression from this:

	(eq
	  (eq nil ())
	  (eq () nil))

	to this:
	
	(eq
	  t
	  (eq () nil))

	then this:

	(eq
	  t
	  t)

	and finally:

	t

	Lisp evaluates expressions recursively by computing the values of the deepest
	argument expressions before computing arguments that are higher up.

	Lisp has some useful built-in functions.  An important function is quote.  It
	takes a single argument and returns it unevaluated.

	For example, when we type our salad list into Lisp:

	(lettuce tomato oil vinegar)

	Lisp returns an error about "lettuce" not being a function.

	To make it clear that our salad list is a list and not an applicaton of the
	lettuce function, we enter:

	(quote (lettuce tomato oil vinegar))

	Lisp responds with:

	(lettuce tomato oil vinegar)

	Lisp wouldn't be useful if we couldn't create our own functions.  To describe a
	function, we use a lambda expression.  A lambda expression takes a list of
	parameters then a sequence of S-expressions to evaluate.

	Here's an example of a lambda function that accepts a single parameter to
	compute its equality with nil:

	(lambda (x) (eq nil x))

	Each parameter in the parameter list is a symbol that represents an argument at
	application time.  The sequence of S-expressions in the lambda may refer to
	these parameters. In fact, it's good practice to make sure that the
	S-expressions in a lambda refer only to the lambda parameters.

	Lisp treats the last S-expression in the lambda specially.  Its value is the
	value that the lambda returns when Lisp applies it.

	In our lambda above, (x) is the list of parameters.  In this case, we have a
	single parameter, x.  The S-expression (eq nil x) is the only expression in our
	lambda.  It's also the last expression, so the lambda returns the value of this
	expression when we apply the lambda.

	When you enter a lambda by itself:

	(lambda (x) (eq nil x))

	Lisp returns the lambda, unapplied: 

	(lambda (x) (eq nil x))

	If we want to apply our lambda to an argument, we need to use the same form as a
	function application:

	(function argument ...)

	We just need to replace function with a lambda expression.

	For example, if we enter:

	((lambda (x) (eq nil x))
	  (quote (lettuce tomato oil vinegar)))

	Lisp applies our lambda like a regular function application by following these
	steps:

	1. Evaluate the expressions of the arguments. 

	2. Bind each evaluated argument, in the order it appears in the application, to
	each of the parameters in the order that they appear in lambda parameter list.

	3. Evaluate each expression in the lambda.  When an expression refers to a
	parameter, Lisp evaluates it by substituting its bound value from step 2.

	4. Return the value of the last expression in the lambda and unbind its
	parameters.

	In our example above, in step 1, Lisp first evaluates the lone argument:

	(quote (lettuce tomato oil vinegar))

	which gives:

	(lettuce tomato oil vinegar)

	In step 2, Lisp binds this evaluated argument to the parameter, x. 

	For step 3, Lisp evaluates the lambda body, replacing occurrences of x with the
	value it is bound to. This:

	(eq nil x)

	becomes:

	(eq nil (lettuce tomato oil vinegar))

	Our argument, (lettuce tomato oil vinegar), is not nil, so eq returns nil. Since
	this is the only and last expression in the lambda, the lambda application
	returns nil.  Returning nil for a non-nil argument is ironic until you remember
	that nil means false in this case.
	
	The parameter bindings in a lambda application last only during the application
	of the lambda.  In step 4, Lisp unbinds the lambda's parameters.  The x argument
	has no value.

	So entering this expression after applying our lambda:

	x

	gives an error about an unbound variable.

	Outside of a lambda, we can bind a symbol to a value so that when you enter the
	symbol, Lisp returns the value.  For example this expression binds "name"
	to Valerie:

	(define name (quote Valerie))

	So entering this expression:

	name

	evaluates to what you would expect:
	
	Valerie

	You can also change an existing binding:

	(define name (quote Isabelle))

	Bindings in a lambda temporarily override outside bindings.  For example:

	(define name (quote James))

	((lambda (name) name) (quote Rose))

	returns:

	Rose

	Inside the lambda application, the name symbol is bound to Rose.  When the
	lambda application returns, the previous binding to name is restored, so that
	entering this expression:

	name

	returns this:

	James

	Of course, you can also bind a lambda to a symbol, which makes the lambda easier
	to use if you intend to refer to it frequently.  For example, comparison with
	nil is something we see enough that we define a handy function for it:

	(define null (lambda (x) (eq x nil)))

	Now comparison to nil is more convenient:

	(null (quote Gus))

	returns:

	nil

	Notice that the define function plays by different rules than a normal function.
	Instead of evaluating its first argument, define takes it literally, as if it
	were quoted.  Therefore by definition (ahem), define isn't a true function.  In
	Lisp, we call this a "special form".  If you paid enough attention earlier, you
	noticed that quote and lambda are also special forms.  Lambda doesn't evaluate
	its list of parameters. And Lisp delays the evaluation of a lambda's expressions
	until it applies the lambda.

	Another special form is cond, which is short for "conditional".  It takes a list
	of clauses.

	(cond clause1 clause2... clauseN)
	
	Each clause is a list of 2 expressions:

	(test result)
	
	If the test expression is true, then cond returns the value of the corresponding
	result expression and stops processing subsequent clauses.  If the test is
	false, cond proceeds to the next clause.  It continues doing so until it finds a
	test that returns true or there are no more expressions.

	For example, to evaluate this expression:

	(cond
	  ((eq name (quote Valerie))  (quote funny))
	  ((eq name (quote James))    (quote silly))
	  (t                          (quote goodbye)))

	Lisp starts with the first clause:

	((eq name (quote Valerie))  (quote funny))

	which has this test expression:

	(eq name (quote Valerie))

	Given our most recent binding for name, this test evaluates to false.  Lisp
	skips to the next clause:

	((eq name (quote James)) (quote silly))

	The test expression evaluates to true.  So Lisp evaluates the second
	expression in this clause, which returns:

	silly

	which becomes the value of our cond expression.  Lisp ignores subsequent
	clauses, so the clause:

	(t (quote goodbye))
	
	doesn't get evaluated.
	
	As a matter of good habit, we always put a final clause in a cond that has t for
	a condition clause's test.  That way we assure ourselves that a cond test
	returns a value that we specify when all other clause tests are false.

	Lisp isn't all special forms.  In fact, there are only a handful.  Most Lisp
	built-in functions evaluate arguments normally.

	For example, the most frequently-used Lisp functions are car and cdr.  For now,
	we partly describe what they do, with full details of their awesomeness later.

	The car function returns the first item in a list and cdr returns a list without
	its first item.  Helpful synonyms for these functions are "first" and "rest",
	respectively.

	For example, let's define our salad list:

	(define salad (quote (lettuce tomato oil vinegar)))

	Entering this:	

	(car salad)

	returns this:

	lettuce

	And entering

	(cdr salad)

	returns

	(tomato oil vinegar)

	To pick individual items in a list, we combine car and cdr.  Entering:

	(car (cdr salad))

	gives us:

	tomato

	This expression:

	(car (cdr (cdr salad)))

	gives us:

	oil

	Note that car and cdr do not modify the list that is bound to salad.  Car
	doesn't reduce a list to its first item, it only tells you what that first item
	is.  Likewise, cdr doesn't remove the first item from a list, it only tells you
	what a list is without its first item.  So after applying car and cdr to salad,
	salad still has its most recent binding.  Entering:

	salad

	still gives us:
	
	(lettuce tomato oil vinegar)	

	Functions that don't modify bindings other than their own are called "pure
	functions", which is another way of saying that you, the Lisp user, don't have
	to worry about unintended consequences when applying a function.  Function purity
	is an important and useful quality in Lisp programming.

	So that's most of the basics of Lisp that we implement in arpilisp.  We'll cover
	the rest as we go.

	References

	McCarthy, "Recursive Functions of Symbolic Expressions and Their Computation by
	Machine, Part I". MIT. 1960.

	McCarthy, et al. "Lisp I Programmer's Manual." MIT. 1960.

	________________________________________________________________________________

	The Shortest Introduction to ARM Assembly Language

	Today's computers have ample capacity to let us pile on layers of operating
	systems, shells, libraries, frameworks, sandboxes, and containers.  So much that
	we've successfully buried assembly language completely out of mind.  But the
	machine is still there, and not as inaccessible, alien, or irrelevant as you
	might believe.

	The Raspberry Pi uses an ARM processor.  The ARM registers that we use in
	arpilisp are 32 bits wide.  Registers r0 to r12 are general-purpose and registers
	r13 to r15 are single-purpose, at least for our purposes.   Register r13 is the
	stack pointer (sp), r14 is the link register (lr), and r15 the program counter
	(pc).

	An extra register, the processor status register (apsr), offers a few single-bit
	condition flags that optionally record effects of previous instructions.

	To manipulate data, like arithmetic, comparison, bit fiddling, and
	so on, ARM requires that we use registers.  ARM has no instructions to
	manipulate data in memory.  Before we operate on something, we first need to
	load it from memory to a register.  In fact, we can't even directly load from or
	store to memory.  We need to load an address into a register then use that
	register to specify the location of a load (ldr) or store (str) operation.

	Side note: Yes, loading an address before loading the contents of the address
	seems like a chicken and egg situation.  It's not, of course.  The assembler and
	the processor play some convenient tricks to solve this problem for us.

	In GNU assembler, constants begin with a pound sign (#).  Addresses start with an
	equal sign (=).

	Labels end with a colon (:).  Labels must be unique with one exception: numeric
	labels.  An assembly file can repeat numeric labels, which makes them handy for
	throw-away labels in an assembly procedure.  An instruction refers to a
	numerical label by post-fixing an "f" or "b", which refers to the matching
	numerical label "forward" or "before" the instruction.

	ARM, the company, and others specify calling conventions and ABIs, but we'll
	ignore them to keep things simple.  Arpilisp is for learning Lisp and assembler,
	not for conforming.

	References

	https://en.wikipedia.org/wiki/IBM_704

	ARM Limited. "ARMv6-M Architecture Reference Manual." 2010.

	________________________________________________________________________________

	Targeting the Processor

	Let's get started.  We begin at the foundation then go on until we get to the
	end: a working Lisp interpreter.

	We target the ARMv6 instruction set, which works with all variants of the
	Raspberry Pi up to the Pi 3.

	*/

	.arch armv6

	/*
	________________________________________________________________________________

	Procedures in Assembly

	These assembler macros tell the assembler, the debugger, and us where a
	procedure starts and ends.  They also make sure that the assembler aligns
	instructions properly and places constant values that our procedure refers to in
	a place that the procedure can reach.

	Reference

	https://community.arm.com/docs/DOC-9652

	*/

	.macro PROC name
	.text
	.balign 4
	.func \name, \name
\name\():
	.endm

	.macro ENDPROC
	.pool
	.endfunc
	.endm

	/*
	________________________________________________________________________________

	Errors

	We need a way to flag an error. We'll use the overflow (V) flag in the apsr.
	This gives us a simple mechanism to set and detect error conditions.  Reacting
	to an error is as simple as appending the "vs" or "vc" condition code to
	affected instructions.

	Our error-flagging method is blunt--it clears all condition flags along with
	setting or clearing V.  As long as we keep that in mind, we won't get stung.

	*/

	.macro ERRSET errsym
	msr apsr_nzcvq, #1<<28
	ldr r7, =\errsym
	.endm

	.macro ERRCLR
	msr apsr_nzcvq, #0
	.endm

	/*
	________________________________________________________________________________

	Returning boolean results

	We use the Z flag in the status register to handle true and false results.  When
	we set the Z flag (and bluntly clear other conditions), we can test for it with
	the EQ condition flag.  Otherwise, we clear the status registers to use the NE
	condition.

	*/
	.macro ZSET
	msr apsr_nzcvq, #1<<30
	.endm
	
	.macro ZCLR
	msr apsr_nzcvq, #0
	.endm

	/*
	________________________________________________________________________________

	Lisp stack

	We need a safe place to store intermediate Lisp values.  We use a stack for
	this.  We cover the idea of "safe" later on.

	We reserve some memory for the Lisp stack and r6 for the Lisp stack pointer.
	This stack pointer is separate from processor's stack pointer, aka sp, aka r13.

	Side note: The Linux kernel conveniently sets up sp for us and reserves some
	memory for the process stack when it launches arpilisp.

	*/
	.equiv LISPSTACKMAX, 1000	/* Number of pointers to Lisp objects. */

	.bss
	.balign 4
lispstackbottom:
	.space LISPSTACKMAX * 4
	.equiv lispstacktop, .

	/*

	Push the Lisp value in r0 onto the Lisp stack.  Signal an error if the stack is
	full.  Generating an error instead of panicking is fine because it lets our
	interpreter gracefully recover from exhausted Lisp stack space.

	*/
	
	PROC pushlisp
	push { r1 }
	ldr r1, =lispstackbottom
	cmp r6, r1
	bne 10f
	ERRSET errstackfull
	b 999f

10:	stmfd r6!, { r0 }
	ERRCLR

999:	pop { r1 }
	mov pc, lr
	ENDPROC

	/*

	Pop from the Lisp stack and store it in r0.  Modifies the condition flags.

	Panic if we try to pop from an empty stack.  We panic instead of generating an
	error because a pop requires a previous push.  In other words, a mismatched pop
	implies a bug in the interpreter's logic, not the Lisp program that it is
	interpreting.

	*/

	PROC poplisp

	ldr r0, =lispstacktop
	cmp r6, r0
	bne 10f
	ldr r0, =panicstackempty
	b panic

10:	ldmfd r6!, { r0 }
	mov pc, lr
	ENDPROC

	/*
	________________________________________________________________________________

	The mighty cons

	Lisp lists are implemented as cons cells.  The cons cell is the most elegant and
	expressive data type ever discovered in computing.  If symbols are the atoms in
	the Lisp universe, then cons cells organize the universe into molecules,
	galaxies, and everything in between.

	A cons cell is a pair of pointers.  Incidentally, "pair" is often used as a
	synonym for a cons cell.

	Remember car and cdr, the functions that return different parts of a
	list?  It turns out that these weird function names are based on the traditional
	names for the pointers in the cons cell.

	cons cell
	+-----+-----+
	| car | cdr |
	+-----+-----+

	It also turns out that stating that the car and cdr functions operate on lists
	is true only at a superficial level.  What these functions really do is operate
	on cons cells.  This makes implementing the car and cdr functions dead easy:
	return the pointer to the object stored in the car or cdr part of the cell,
	respectively.

	In a cell, the car and cdr pointers may each point to another cell, a symbol, or
	nil.

	For example, here is a cell where the car points to a symbol and the cdr points
	to nil.

	+-----+-----+
	| car | cdr ---> nil
	+--|--+-----+
	   |
	   v
	symbol

	For simplicity of implementation, a practical value for nil is 0.  We store
	nothing useful at memory location 0.

	Side note: In fact, a Linux user program cannot store or load location 0.  The
	ARM Linux kernel doesn't allow it by design.  Doing so results in a segmentation
	fault, which is an intentional thing.

	Reference

	https://www.kernel.org/doc/Documentation/arm/memory.txt

	*/
	.equiv NIL, 0

	/*

	We keep all our cells in a pool.  A cell occupies 2 words = 2 * 4 bytes = 8
	bytes.

	*/
	.bss
	.balign 4

	.equiv CELLMAX, 10000

cells:
	.space CELLMAX * 2 * 4
	.equiv cellsend, .

	/*

	A Lisp program can ignore its obsolete data by simply abandoning it.  From a
	Lisp programmer's point of view, memory is infinite.  In fact, Lisp provides no
	explicit way for a program to deallocate memory.

	Side note: I know that you know that I know that limitless memory is impossible.
	And it's easy to write a Lisp program that quickly exhausts memory.  I'm just
	saying that limitless memory is part of the Lisp programming model.

	In assembly language, memory is definitely limited.  We have to meticulously and
	precisely manage memory usage.  When we no longer need it, we have to carefully
	track it to reuse it later.  

	So it's up to our interpreter to implement automatic memory management.  When a
	Lisp program runs out of memory, the interpreter must look for abandoned memory
	to reuse.  For this, McCarthy's team invented the term "garbage collection".  The
	name stuck.

	The first Lisp used mark-sweep garbage collection.  So do we because it is
	simple to describe and implement.  Also, we limit ourselves to collect abandoned
	cons cells.  We won't bother with abandoned symbols.

	There are two ingredients to marking and sweeping: a free list and a root set.

	First ingredient: the free list.  The free list points to abandoned cells that
	are ready to use.  When a Lisp program needs a new cons cell, Lisp removes it
	from the free list and gives it to the program.

	*/

freelist:
	.word NIL

	/*

	When we start the interpreter, we have to build the free list.  We just iterate
	through the cell pool, pointing each cons cell's cdr to the next cons cell.  At
	the last cons cell, we point to nil.

	When it's initialized, our free list initially looks like this:
	
	             +-----+-----+   +-----+-----+            +-----------+
	freelist --->| car | cdr --->| car | cdr ---> ... --->| car | cdr --->nil 
	             +-----+-----+   +-----+-----+            +-----+-----+   

	*/

	PROC initfreelist

	push { r0-r2, lr }
	ldr r0, =cells
	ldr r1, =freelist		/* Free list starts at the beginning */
	str r0, [ r1 ]			/* of the cell pool. */

	ldr r2, =cellsend
	sub r2, r2, #8			/* Stop before the last cell. */

1:	mov r1, r0
	add r0, r0, #8			/* The next cell. */
	str r0, [ r1, #4 ]
	cmp r0, r2
	bne 1b

	pop { r0-r2, pc }

	ENDPROC

	/*

	Second ingredient in garbage collection: the root set.  The root set is the
	starting point for marking.  Lisp follows the cells pointed to by the cells in
	the root set, travelling through all referred cells.

	In other words, all cells that are reachable from the root set, directly or
	indirectly, are safe from garbage collection.  Cells that are not reachable are
	added to the freelist.

	The garbage collector starts by marking all used cells.  This is the mark in
	mark-and-sweep.

	During the mark phase, Lisp could encounter a cell that it has already
	marked.  In this case, Lisp stops chasing further because we know that
	subsequent, reachable cells from a marked cell are already marked.

	To mark a cons cell, we must make sure not to corrupt the original data in the
	cell.  We do this by taking advantage of a quirk in ARM's data alignment
	preferences.  Genuine ARM pointers are aligned to the nearest word (4 bytes), so
	a pointer's 2 least significant bits are always 0.

	ARM pointer
	+--------+--------+--   --+-------+-------+
	| 0 or 1 | 0 or 1 |  ...  |   0   |   0   |
	+--------+--------+--   --+-------+-------+
	  bit 31   bit 30           bit 1   bit 0 
	
	We take advantage of these 2 bits to encode information about a Lisp object in
	the actual pointer to the object.

	To specify that a cell is in use during the mark phase, we mark its car by
	settings bit 1 to 1.  We have another use for bit 0, covered later.

	We don't need to worry about decoding a marked cell to refer to its pointer for
	a few reasons: our Lisp program is suspended during mark and sweep, we don't follow
	reachable cells from a marked cell, and we only use the mark bit during the
	marking phase of garbage collection.

	*/
	.equiv MARKMASK, 0b10

	PROC collectgarbage

	push { r0, r6, lr }

	/* Mark the root set. */
	mov r0, r9			/* The environment. */
	bl mark
	mov r0, r8			/* The expression being evaluated. */
	bl mark
	mov r0, r7			/* The value. */
	bl mark
	mov r0, r5			/* The list of lambda argument values. */
	bl mark
	ldr r0, =freelist
	ldr r0, [r0]
	bl mark

	/* Mark the Lisp stack. */
10:	ldr r0, =lispstacktop	
	cmp r6, r0
	beq 20f
	ldmfd r6!, { r0 }
	bl mark
	b 10b

20:	bl sweep
	pop { r0, r6, pc }

	ENDPROC

	/*

	Given a pointer in r0, mark it if applicable.

	*/

	PROC mark

	push { r0-r3, lr }

	/* Does r0 point to a cell? */
1:	cmp r0, #NIL
	beq 999f
	tst r0, #SYMMASK
	bne 999f

	/* Is the car already marked? */
	ldr r1, [ r0 ]
	tst r1, #MARKMASK
	bne 999f

	/* Mark the car. */
	mov r2, r1			/* Mark with r2, save r1. */
	orr r2, r2, #MARKMASK
	str r2, [r0]			/* Store the mark in car. */

	/* Follow the data pointed to by the car and cdr. */
	mov r3, r0			/* Save original pointer in r3. */
	mov r0, r1			/* Chase after the car. */
	bl mark
	ldr r0, [ r3, #4 ]		/* Chase after the cdr. */
	b 1b

999:	pop { r0-r3, pc }
	ENDPROC

	/*

	With marking completed, the interpreter starts sweeping.  It iterates through
	the cell pool, unmarking active cells and inserting inactive cells into the free
	list.

	*/
	PROC sweep

	push { r0-r1, lr }
	ldr r1, =cells
1:	ldr r0, [ r1 ]			/* Is the car marked? */
	tst r0, #MARKMASK
	beq 2f

	/* Cell is active, unmark it. */
	bic r0, #MARKMASK
	str r0, [ r1 ]
	b 3f

	/* Cell is inactive, add it to freelist. */
2:	mov r0, #0			/* Clear the car. */
	str r0, [ r1 ]
	ldr r0, =freelist		/* Store freelist head in the cdr. */
	ldr r0, [ r0 ]
	str r0, [ r1, #4 ]
	ldr r0, =freelist		/* Point freelist to the new head. */
	str r1, [ r0 ]

	/* Next cell in the cell pool. */
3:	ldr r0, =cellsend
	add r1, r1, #8
	cmp r1, r0
	bne 1b

	pop { r0-r1, pc }

	ENDPROC

	/*

	Lisp provides the cons function to allocate new cells.  A Lisp program uses cons
	to construct its data.  This is where the free list comes in.
	
	             +-----+-----+   +-----+-----+            +-----------+
	freelist --->| car | cdr --->| car | cdr ---> ... --->| car | cdr --->nil 
	             +-----+-----+   +-----+-----+            +-----+-----+   
		         cell 1          cell 2                   cell N


	Allocating a cons cell from the freelist is simple.  We just "pop" it from the
	freelist:
	
                     allocated cell
	                   |
	                   V
	             +-----+-----+     +-----+-----+            +-----------+
	freelist -+  | car | cdr |  +->| car | cdr ---> ... --->| car | cdr --->nil 
	          |  +-----+-----+  |  +-----+-----+            +-----+-----+   
		  |      cell 1     |     cell 2                   cell N
		  |		    |	
	          +-----------------+
	
	Given pointers to Lisp objects in r1 and r2, return an allocated cell in r0 such
	that its car contains r1 and cdr contains r2.  Otherwise, panic if there is no
	memory or issue an error if the Lisp stack is full.

	*/

	PROC cons

	push { r3-r4, lr }
	ldr r3, =freelist		/* Is the free list empty? */
	ldr r0, [ r3 ]
	cmp r0, #NIL
	bne 10f

	
	mov r0, r1
	bl pushlisp
	bvs 999f
	mov r0, r2
	bl pushlisp
	bvs 999f
	bl collectgarbage
	bl poplisp
	mov r2, r0
	bl poplisp
	mov r1, r0

	ldr r0, [ r3 ]			/* Is the free list still empty? */
	cmp r0, #NIL
	bne 10f
	
	ldr r0, =panicmemfull
	b panic

10:	ldr r4, [ r0, #4 ]		/* Advance freelist. */
	str r4, [ r3 ]
	str r1, [ r0 ]			/* Store the car and cdr. */
	str r2, [ r0, #4 ]
	ERRCLR
999:	pop { r3-r4, pc }

	ENDPROC

	/*
	________________________________________________________________________________

	Symbols

	In arpilisp, our simplest object, after the empty list, is the symbol.  A symbol
	is just a reference to the character string of itself.

	Side note: Most other Lisps allow the programmer to attach properties to a
	symbol.  For example, a symbol has a property called the print name, which is
	the textual representation of the symbol.  To print a symbol, the Lisp
	interpreter retrieves the print name property and outputs it.  The property
	mechanism is overkill for arpilisp.  Instead, we use a symbol as a reference to
	its print name.

	The format we use for print names is the length of its print name encoded as a
	word-aligned word, followed by the character string of the print name itself.
	The next print name follows after that, aligned to a word boundary, and so on.

	symbol 1			 	 symbol 2
	+-----------+------------+--------------+---- - - -
	| length    | characters | padding      | length
	+- 4 bytes -+- n bytes --+- < 4 bytes  -+---- - - -

	We store symbol print names in a block of memory.  The traditional name for this
	block is "obarray."

	*/

	.equiv OBARRAYMAX, 2000	/* Multiple of 4, < 2^30. */

	.data
	.balign 4
obarray:

	/* Keep track of the last unused position in obarray. */
	.balign 4
	.set OBARRAYEND, .

	/*

	We have 2 types of object: symbols and cells.  We need a way to determine if a
	pointer refers to an object that is appropriate for the current computation.  For
	example, if a Lisp function expects a symbol as an argument, we want to ensure
	that the argument is indeed a symbol and not a cell or nil.

	We mentioned earlier that the least 2 significant bits in an ARM pointer are 0.
	We already use bit 1 as the mark bit for garbage collection.  That leaves us
	with the least significant bit, bit 0, to specify the type of object that a
	pointer refers to.  When bit 0 is 1, the pointer refers to a symbol, not a cell
	and not nil either.

	*/

	.equiv SYMMASK, 1

	/*

	A reference to a symbol is not really a pointer.  To decode a reference to a
	symbol into a usable pointer, we mask out the flag and add the address of
	obarray.
	
	Why not use a real pointer instead of addding the address of obarray?  It turns
	out that setting up the initial symbols we need for arpilisp poses a challenge.
	Ideally, we would like to pre-fill obarray at assembly time instead of runtime.
	But the assembler won't let us perform assembler-time arithmetic on labels
	because the ELF binary format and the Linux kernel conspire to force the
	calculations of final addresses only at runtime.

	We could hard-code the addresses of Lisp symbols at assembly time, but we'd have
	to either make arpilisp more complicated or depend on some uncomfortable
	assumptions about the memory map of a Linux process.

	However, the assembler happily lets us compute offsets from assembly labels.
	Our solution, then, is to treat a Lisp symbol pointer not as a pointer but as an
	offset into obarray.

	The LISPSYM macro appends a symbol entry to obarray.  We use it to pre-define
	our initial set of symbols.  We may only apply this macro immediately
	after the definition of obarray.  So pre-define all your symbols here.

	*/
	.macro LISPSYM sym, printname
	.data
	.balign 4
	.equiv \sym , (. - obarray) | SYMMASK
	.word \sym\()_STREND - \sym\()_STRSTART
	.balign 4
	.equiv \sym\()_STRSTART, .
	.ascii "\printname"
	.equiv \sym\()_STREND, .
	.balign 4
	.set OBARRAYEND, .
	.endm

	LISPSYM symnil, "nil"
	LISPSYM symt, "t"
	LISPSYM symquote, "quote"
	LISPSYM symatom, "atom"
	LISPSYM symeq, "eq"
	LISPSYM symcar, "car"
	LISPSYM symcdr, "cdr"
	LISPSYM symcons, "cons"
	LISPSYM symcond, "cond"
	LISPSYM symdefine, "define"
	LISPSYM symlambda, "lambda"
	
	LISPSYM errdot, "Error: unexpected dot "
	LISPSYM errparenmissing, "Error: expected closing parenthesis "
	LISPSYM errparen, "Error: unexpected closing parenthesis "
	LISPSYM errbadlistexpr, "Error: unknown expression "
	LISPSYM errcellornil, "Error: expected a cell or nil "
	LISPSYM errbindsym, "Error: expected a symbol to bind "
	LISPSYM errbadsym, "Error: expected a valid symbol "
	LISPSYM errunboundvar, "Error: unbound variable "
	LISPSYM errargextra, "Error: too many arguments "
	LISPSYM errargmissing, "Error: missing arguments "
	LISPSYM errstackfull, "Error: stack full "
	
	LISPSYM panicstackempty, "Panic: stack empty"
	LISPSYM panicobarrayfull, "Panic: obarray full"
	LISPSYM panicmemfull, "Panic: memory full"

	/* Fill the rest of the obarray space. */
	.space OBARRAYMAX - (. - obarray)
	.set OBARRAYCAP, .

	/* Remember the last unused position in obarray. */
obarrayend:
	.word OBARRAYEND

	/*

	A useful operation for symbols is to compare them for equality.  Given two
	symbols, are they the same?

	One way to do this is to compare their print names, character by character.

	A much simpler and faster way is to compare the symbols' pseudo-pointers.

	Remember the eq function earlier?  Implementing this function in assembler is
	dead easy: just compare two pointers.  For cells and nil, this work well.  We
	would like to have this same convenience and efficiency for symbol comparison.
	But there's a down side: for this simple symbol comparison, we need to make sure
	that symbols are unique, which means making sure that we only record a single
	copy of a symbol.

	Side note: The up side of storing a single copy of a print name is that we save
	memory, which is, incidentally, why we don't bother with garbage collection of
	symbols.

	In Lisp, we say that this storage of only one instance of a symbol is
	"interning" a symbol.  To create a new symbol, either return a reference to the
	single instance of a symbol in obarray or, if the symbol has not yet been
	interned, create the symbol in obarray and return the reference.

	The buffer for a symbol to intern is internbuffer.  We use it as a temporary
	place to store a symbol from input.  We structure it like a symbol in obarray,
	which means a word-aligned word-sized length value immediately followed by a
	character string.

	*/
	.equiv INTERNMAX, 32		/* A multiple of 4 */

	.bss
	.balign 4
internbufferlen:
	.word 0
	.balign 4
internbuffer:
	.space INTERNMAX

	/*

	Return the pseudo-pointer to the symbol in obarray that matches the string in
	internbuffer.  If the symbol does not exist, create it in obarray and return the
	symbol's pseudo-pointer.

	*/

	PROC intern

	push { r0-r5, lr }

	/* Search for the symbol in obarray. */
	ldr r0, =obarray		/* Start of obarray. */

1:	ldr r1, =obarrayend		/* At the end of obarray? */
	ldr r1, [ r1 ]
	cmp r0, r1
	beq 5f

	mov r1, r0
	ldr r2, =internbufferlen

	/* Compare print name lengths. */
	ldr r3, [ r1 ], #4		/* Length of print name in obarray. */
	ldr r4, [ r2 ], #4		/* Length of print name to intern. */
	cmp r3, r4
	bne 4f

	/* Compare print names. */
2:	cmp r3, #0			/* Have we compared all characters? */
	beq 8f				/* Found.  Symbol already interned. */

3:	ldrb r4, [ r1 ], #1
	ldrb r5, [ r2 ], #1
	cmp r4, r5
	bne 4f
	sub r3, r3, #1
	b 2b

	/* Advance to the next print name in obarray. */
4:	ldr r1, [ r0 ]			/* Length of the current string. */
	add r1, r1, #4			/* Advance to the first character. */
	add r1, r1, r0			/* Add string's address. */

	/*

	At this point, r1 is at the end of the symbol object that r0 points to.  The next
	symbol object is at the next 4-byte boundary.  We need to round up to this
	boundary by calculating how much to add to r1.  The bad news is that ARM doesn't
	have an integer modulo instruction.  The good news is that 4 is a power of 2, so
	bit manipulation is the obvious choice and ARM has plenty of useful
	bit-manipulation instructions.

	To calculate an offset to add to r1, we want this mapping of the least 2 bits:

	0b00 -> 0b00
	0b01 -> 0b11
	0b10 -> 0b10
	0b11 -> 0b01

	Note that for 0b00, we're already there so there's nothing to add to r1.

	*/

	and r0, r1, #0b11		/* Consider only the least 2 bits. */
	rsb r0, r0, #4			/* Subtract 4. */
	and r0, r0, #0b11		/* Keep only the least 2 bits. */

	add r0, r0, r1			/* Point r0 to next object. */

	b 1b

	/*

	Allocate a new print name in obarray.

	At this point, both r0 and r1 contain the value stored at obarrayend.  This
	value points to our new symbol, which we return in r0.  So we preserve r0 for
	that purpose.

	*/

5:	ldr r3, =internbufferlen

	/* Is there room in obarray? */
	add r1, r1, #4			/* Add the word for the length. */
	ldr r2, [ r3 ]
	add r1, r1, r2			/* Add the length of the string. */
	and r2, r1, #0b11		/* Align to the next 4-byte boundary. */
	rsb r2, r2, #4
	add r1, r1, r2
	ldr r2, =OBARRAYCAP
	cmp r1, r2
	blt 6f

	/* No more symbol room. */
	ldr r0, =panicobarrayfull
	b panic

	/* Expand obarray. */
6:	ldr r4, =obarrayend
	str r1, [ r4 ]

	mov r1, r0			/* Reset r1 to our new symbol. */

	ldr r2, [ r3 ], #4		/* Copy the length. */
	str r2, [ r1 ], #4

	/* Copy the string. */
7:	cmp r2, #0
	beq 8f
	ldrb r4, [ r3 ], #1
	strb r4, [ r1 ], #1
	sub r2, r2, #1
	b 7b

	/* Encode our pointer as a symbol. */
8:	ldr r1, =obarray		/* Subtract obarray base. */
	sub r7, r0, r1
	orr r7, r7, #SYMMASK		/* Imprint the mask. */

	pop { r0-r5, pc }

	ENDPROC

	/*

	________________________________________________________________________________

	Output

	We need to print symbols, cells, and nil.  Here is where we get to see the
	internal representations of cons cells and symbols meet their external Lisp
	counterparts.  We've seen hints earlier, but now we get to clearly see how lists
	are made of cons cells.

	To print a pointer to a Lisp object, let's take a first stab at syntax rules:

	pointer = 'nil' | symbol | cell 

	cell = '(' car '.' cdr ')'

	car = pointer

	cdr = pointer

	A few things about the first draft of our syntax rules:

	* A cell's textual representation starts and ends with parentheses.

	* We separate the car and cdr with a period.  In Lisp, we call this "dot
	notation".

	* A list is a recursive definition; a car and cdr may each point to other cells
	via the syntax rule, pointer. 

	Example output from these rules:

	* Printing nil produces the output of "nil".

	* Printing a symbol produces the symbol's character string.

	* Let A be a cons cell with car pointing to symbol "abacus" and cdr pointing to
	symbol "anchovy".  Output: "(abacus . anchovy)".

	A
	+-----+-----+
	| car | cdr ----> anchovy
	+--|--+-----+
	   |
	   v
	abacus

	* Let B be a cons cell in which the car points to symbol "beluga" and cdr points
	to nil.  Printing B gives "(beluga . nil)".

	B
	+-----+-----+
	| car | cdr -----> nil
	+--|--+-----+
	   |
	   v
	beluga

	* Let C be a cons cell with car pointing to symbol "cantilever" and cdr pointing
	to cell B.  Its output: "(cantilever . (beluga . nil))".

	C                B
	+-----+-----+  	 +-----+-----+
	| car | cdr ---->| car | cdr ----> nil
	+--|--+-----+	 +--|--+-----+
	   |		    |
	   v		    v
	cantilever	 beluga

	These format rules are perfectly useful and accurate.  But notice that they
	don't quite give the output for lists that we saw earlier.  Our rules produce a
	lot of dots and parentheses for cons cells structures that barely approach
	complexity.

	Now you know why Lisp is sometimes referred to as "Lots of Irritating, Stupid
	Parentheses."

	Let's add a couple of rules to make the output a little cleaner. 

	Our new rules:

	pointer = 'nil' | symbol | cell 

	cell = '(' cell-contents ')'

	cell-contents = car (cdr-nil | cdr-cell | cdr-symbol)

	car = pointer

	cdr-nil = empty-string

	cdr-cell = ' ' cell-contents

	cdr-symbol = ' . ' symbol

	Note our changes:
	
	* When the cdr is nil, don't print the dot and don't print the 'nil'.

	* When the cdr points to a cell, we don't print a dot or the left
	parenthesis of the cons cell that the cdr points to.

	* Only when the cdr points to a symbol, do we print a dot, followed by the
	symbol.
	
	With these updated rules our example output becomes:

	* Nil is still output as "nil".

	* A symbol is still output as itself.

	* A: "(abacus . anchovy)"

	* B: "(beluga)"

	* C: "(cantilever beluga)"

	Now our output rules are consistent with what we expect about list output.

	Before we start printing, let's define some character strings to match our
	rules.

	*/

	.balign 4
leftparenstr:
	.ascii "("
	.equiv LEFTPARENSTRLEN, . - leftparenstr

	.balign 4
rightparenstr:
	.ascii ")"
	.equiv RIGHTPARENSTRLEN, . - rightparenstr

	.balign 4
spacestr:
	.ascii " "
	.equiv SPACESTRLEN, . - spacestr

	.balign 4
dotstr:
	.ascii " . "
	.equiv DOTSTRLEN, . - dotstr

	/*

	Most Lisp implementations provide prin1 and print functions.  The prin1
	procedure outputs a Lisp object without a newline ending.

	Given a pointer to a Lisp object in r0, output its textual representation.

	*/

	PROC prin1

	push { r0-r3, lr }

	cmp r0, #NIL			/* Is this nil? */
	bne 1f

	/* Print nil. */
	ldr r0, =symnil
	b 2f

1:	tst r0, #SYMMASK		/* Is this a symbol? */
	beq 3f

	/* Print a symbol. */
2:	bic r0, #SYMMASK
	ldr r1, =obarray
	add r0, r0, r1
	add r1, r0, #4			/* Symbol string address. */
	ldr r2, [ r0 ]			/* Symbol length. */
	bl write
	b 999f				/* Done. */

	/* Print a cell. */
3:	ldr r1, =leftparenstr
	ldr r2, =LEFTPARENSTRLEN
	bl write

4:	mov r3, r0			/* Save our cell pointer. */
	ldr r0, [r3]			/* Get the car and print it. */
	bl prin1
	ldr r0, [r3, #4]		/* Get the cdr. */
	cmp r0, #NIL			/* Is the cdr nil? */
	beq 6f
	tst r0, #SYMMASK	/* Is the cdr a symbol? */
	bne 5f
	ldr r1, =spacestr		/* Cdr is a cell, so iterate. */
	ldr r2, =SPACESTRLEN
	bl write
	b 4b

5:	ldr r1, =dotstr			/* Cdr is a symbol, so print the dot. */
	ldr r2, =DOTSTRLEN
	bl write
	bl prin1			/* Print the symbol. */

6:	ldr r1, =rightparenstr
	ldr r2, =RIGHTPARENSTRLEN
	bl write

999:	pop { r0-r3, pc }

	ENDPROC

	/*

	Print an end-of-line.  In Lisp, outputing an end-of-line is traditionally
	referred to as "terminate the print line" or "terpri."

	*/

	.data
	.balign 4
eolstr:
	.ascii "\n"
	.equiv EOLSTRLEN, . - eolstr

	PROC terpri

	push { r1, r2, lr }
	ldr r1, =eolstr
	ldr r2, =EOLSTRLEN
	bl write
	pop { r1, r2, pc }

	ENDPROC

	/*

	Print an object with newlines around it.

	*/

	PROC print

	push { lr }
	bl prin1
	bl terpri
	pop { pc }

	ENDPROC

	/*
	________________________________________________________________________________

	Low-Level I/O
	
	With the logic to format S-expressions for output, we need a way to actually get
	this information into the real world for the user to see.  This is the part
	where we implement I/O.

	Notice that the gcc command we use earlier includes the -nostdlib option.  This
	tells gcc not to link to the standard system and startup libraries.  These
	libraries pile layers of convenient wrappers and lots of infrastructure between
	a user program and the kernel's low-level services.

	We choose to avoid that.  Instead, we call the linux kernel directly through its
	system call interface.  Each system call has a unique number.  Most system calls
	also need parameters.  To cross the border between a user program and the
	kernel, we load the system call's number and parameters into registers then use
	the svc instruction to take the leap into the kernel.  The kernel does its magic
	and returns execution to the user program at the next instruction after svc.

	Arpilisp uses a minimum of system calls.  The full list is here:

	/usr/src/arch/arm/kernel/calls.S

	For output, we use sys_write, specified by r7, and the standard output file, in
	r0.  The kernel also requires that we specify the address of the buffer to write
	from in r1 and the length of the buffer in r2.

	*/

	PROC write

	push { r0, r7, lr }
	mov r7, #4			/* sys_write */
	mov r0, #1			/* stdout */
	svc #0
	pop { r0, r7, pc }

	ENDPROC

	/*
	________________________________________________________________________________

	Input

	In Lisp, the read function converts input characters into an S-expression.

	We read a little differently than we print.  Printing is simple: we already
	know everything we need to print.  But when we read, we don't know what it is
	until it's read completely.

	Most books on programming language implementation divide the problem of input
	processing into lexical analysis and parsing.  A lexical analyzer groups
	characters into tokens.  A parser then groups tokens into expressions,
	statements, and the other higher-level constructs of a high-level language.

	S-expression syntax, especially for arpilisp, is simple enough not to worry
	about a formal separation of lexical analysis and parsing.  Lexical analysis for
	arpilisp has only these things to tokenize:

	* opening parenthesis

	* closing parenthesis

	* dot

	* symbol

	The parentheses and dot characters are easy to read: when you read one of them,
	you're done reading the token.

	A symbol token is composed of one or more sequential characters. This is a
	problem because we can read only a single character at a time.  When we get the
	first character that forms a symbol, we have no way to know if we have read all
	of the symbol.  The only way to confirm whether we have read a complete symbol
	or not is to read the next character.

	For example, we read an "h" character.  This "h" might be a complete symbol.
	But it might not be, so we are compelled to keep reading.  We read the next few
	characters, which are "a" then "t" then " " (space).  The space is not part of
	the symbol, which tells us conclusively that the previous three characters form
	the symbol "hat".

	Reading the space means we have gone too far. We have read a character that we
	can't use now but will need later for the next token.  We need some way to
	"unread" this character.

	In Unix-influenced systems, there is exactly such a standard library function,
	ungetc().  This function puts a character back in a file so that it can be
	re-read later.

	It's a funny way to go about things, because the character really isn't put back
	in the file.  The kernel offers no such capability for practical and technical
	reasons.  It's actually the standard library that fakes an "unget".  Implementing
	ungetc() requires some trickery in a higher-level language, and especially
	in assembly language.

	Instead, we use a simpler alternative, excellently described by Jack W. Crenshaw
	in his Let's Build a Compiler articles.  The Crenshaw method uses a look-ahead
	variable, Look, and a function, GetChar.  Look contains the character that the
	lexical analyzer is currently considering.  GetChar updates Look by reading the
	next character in a file.  In other words, only when we are sure that we can use
	the Look character do we bother to read the next character, where it is safely
	held in Look to be analyzed when it's needed.

	Reference

        http://www.compilers.iecc.com/crenshaw/

	*/

	.data
	.balign 4
lookbuffer:
	.ascii "\n"			/* Force a getchar and a prompt. */

	.equiv EOT, 4			/* Ctrl+D, end of transmission. */

	.macro LOOK reg
	ldr \reg, =lookbuffer
	ldr \reg, [ \reg ]
	.endm

	/*

	Before we get to actually reading S-expressions, we need some lower-level
	procedures.

	To read characters from standard input, we use the sys_read system call.

	*/

	PROC getchar

	push { r0-r2, r7, lr }
	ldr r1, =lookbuffer
	mov r2, #1
	mov r7, #3			/* sys_read */
	mov r0, #0			/* stdin */
	svc #0
	cmp r0, #0			/* End of file? */
	moveq r0, #EOT
	streq r0, [ r1 ]
	pop { r0-r2, r7, pc }

	ENDPROC

	/*

	We need procedures to validate characters that we read.  For example, we need to
	know if a character can be used for a symbol.

 	In arpilisp, a valid symbol character is any graphical character that is not
	otherwise part of Lisp syntax.  This means any string of characters is allowed
	in a symbol excluding control characters (which includes white space), the
	opening and closing parentheses, and dot.
 
	This function only works within the Unicode Basic Latin block, aka
	ASCII.  We could make the effort to support Unicode more substantially, but we
	want to focus on Lisp in assembly, not Unicode.
	
	*/

	PROC issym
	push { lr }
	cmp r0, #'('
	beq 20f
	cmp r0, #')'
	beq 20f
	cmp r0, #'.'
	beq 20f
	cmp r0, #EOT
	beq 20f
	cmp r0, #'!'
	blt 20f
	cmp r0, #'~'
	bge 20f
	ZSET				/* A valid symbol character. */
	b 999f
	
20:	ZCLR				/* Not valid. */

999:	pop { pc }
	ENDPROC


	/*

	Is the character in r0 white space?  Notice that we don't use the ZSET and
	ZCLR macros because the cmp instruction sets Z for us.

	*/
	PROC iswhite
	push { lr }
	cmp r0, #' '
	beq 999f
	cmp r0, #'\t'
	beq 999f
	cmp r0, #'\n'
999:	pop { pc }
	ENDPROC

	/*

	We enhance our lexical analyzer to recognize white space for our own human
	convenience.

	*/

	PROC skipwhite

	push { r0, lr }
1:	LOOK r0
	bl iswhite
	bne 999f
	bl getchar
	b 1b
99:	pop { r0, pc }

	ENDPROC

	/*

	Now we're ready to read S-expressions. We use the same syntax rules that we
	use for output.

	Note how we save and restore r6, the Lisp stack pointer.  We do this to simplify
	error handling; instead of being careful about unwinding the Lisp stack to handle
	an error, we just reset r6 to its starting point.

	Also notice how read is a wrapper. It does nothing but save r6 then call read1,
	which does the heavy lifting.  The reason is because of lists: a list may
	contain symbols, nil, and other lists.  To implement this definition, we need a
	procedure that can call itself, read1.

	Read an S-expression, returning a pointer to it in r7.

	*/

	PROC read

	push { r6, lr }
	bl read1
	pop { r6, pc }

	ENDPROC

	/*

	Read an S-expression.

	*/
	PROC read1

	push { r0, lr }
	bl skipwhite
	LOOK r0

	/* End of file.  Return to the OS. */
	cmp r0, #EOT
	beq finish

	/* We aren't ready for a dot here. */
	cmp r0, #'.'
	bne 1f
	bl getchar			/* Eat the dot. */
	ERRSET errdot
	b 999f

	/* We aren't ready to close parenthesis either. */
1:	cmp r0, #')'
	bne 2f
	bl getchar			/* Eat the closing parenthesis. */
	ERRSET errparen
	b 999f

2:	cmp r0, #'('			/* Read a cell? */
	bne 3f
	bl readcell
	bvs 999f
	b 990f

3:	bl issym
	beq 4f
	bl getchar			/* Eat the non-symbol character. */
	ERRSET errbadsym
	b 999f
4:	bl readsym
	bvs 999f
	
990:	ERRCLR
999:	pop { r0, pc }

	ENDPROC

	/*

	Read a cell, returning a pointer to it in r7.

	Remember that our print rule for cell-contents may contain a car, which is a
	pointer, which may refer to a cell.  This mutual recursion means we allow an
	arbitrarily deep number of cars before we get to the cdr-nil, cdr-symbol, or
	cdr-cell rules.

	*/

	PROC readcell

	push { r0-r3, lr }
	bl getchar			/* Eat the opening parenthesis. */
	bl skipwhite

	LOOK r0				/* Immediate closing parenthesis? */
	cmp r0, #')'
	bne 5f
	mov r7, #NIL
	b 990f

	/* Read the first object in our list. */
5:	bl read1
	bvs 999f

	/* Construct a list comprising only of a pointer to our first object. */
	mov r1, r7
	mov r2, #NIL
	bl cons
	mov r7, r0
 	add r3, r0, #4			/* Point to the cdr of our list. */

10:	bl skipwhite			/* Are we at the end of our list? */
	LOOK r0
	cmp r0, #')'
	beq 990f

 	cmp r0, #'.'			/* Are we at a dot? */
 	beq 30f

20:	mov r0, r7			/* Read the next object in our list. */
	bl pushlisp
	bvs 999f
	bl read1
	bvs 999f

	mov r1, r7			/* Append the object's pointer to our list. */
	mov r2, #NIL
	bl cons
	str r0, [ r3 ]			/* Point the cdr to the new cons. */
 	add r3, r0, #4			/* Point to the cdr of our extended list. */
	bl poplisp
	mov r7, r0
	b 10b

30:	bl getchar			/* Eat the dot. */
	mov r0, r7
	bl pushlisp
	bvs 999f
	bl read1			/* Read the cdr. */
	bvs 999f
	str r7, [ r3 ]			/* Store the cdr. */
	bl poplisp
	mov r7, r0

	bl skipwhite		 	/* Expect a closing parenthesis. */
	LOOK r0
	cmp r0, #')'
	beq 990f

	ERRSET errparenmissing
	b 999f

990:	bl getchar			/* Eat the closing parenthesis. */
	ERRCLR
999:	pop { r0-r3, pc }

	ENDPROC

	/*

	Read a symbol starting with the character in r0, returning a pointer to the
	symbol in r7.

	This procedure assumes that, on entry, the character in r0 has been validated by
	issym.

	*/
	PROC readsym

	push { r0-r2, lr }
	mov r1, #0			/* Number of characters in the symbol. */
	ldr r2, =internbuffer
1:	cmp r1, #INTERNMAX		/* A symbol can be longer than */
	bge 2f				/* INTERNMAX but we only recognize */
	strb r0, [ r2 ]			/* the first INTERNMAX characters. */
	add r2, r2, #1
	add r1, r1, #1
2:	bl getchar
	LOOK r0
	bl iswhite
	beq 3f
	cmp r0, #'('
	beq 3f
	cmp r0, #')'
	beq 3f
	cmp r0, #'.'
	beq 3f
	cmp r0, #EOT
	beq 990f

	bl issym
	beq 1b
	
	bl getchar			/* Eat the non-symbol character. */
	ERRSET errbadsym
	b 999f
	
3:	ldr r2, =internbufferlen	/* Finished reading.  Store the length. */
	str r1, [ r2 ]
	bl intern

990:	ERRCLR

999:	pop { r0-r2, pc }

	ENDPROC

	/*

	When the kernel hands the CPU over to an executable file, it calls _start.
	Normally the standard startup libary provides this entry point, so we need to
	furnish our own.

	*/

	.global _start

	PROC _start

	ldr r1, =greeting
	ldr r2, =GREETINGLEN
	bl write
	bl initfreelist
	b repl

	ENDPROC

	.data
	.balign 4
greeting:
	.ascii "arpilisp version 22/7\n\n"
	.equiv GREETINGLEN, . - greeting

	/*

	To terminate arpilisp, we use the sys_exit system call.  This call needs only one
	parameter, the exit status.  Unlike other system calls, the kernel doesn't
	return execution to arpilisp after we make the system call for sys_exit.  So we
	don't bother pushing and popping registers. 

	*/

	PROC finish

	mov r7, #1			/* sys_exit */
	mov r0, #0			/* Exit status. */
	svc #0				/* Call the system. */

	ENDPROC

	/*

	Panic when we reach an error condition that we can't recover from.  Print the
	symbol in r0 and return to the OS.

	*/

	PROC panic

	bl print
	b finish

	ENDPROC

	/*
	________________________________________________________________________________

	The Read Evaluate Print Loop

	Congratulations for getting through the low-level machinery of a Lisp
	interpreter!  Now we can implement higher-level parts of Lisp itself, stepping
	in hybrid territory between assembly and Lisp.

	The Read Evaluate Print Loop (REPL) is self-explanatory: read an S-expression,
	evaluate it, print the value, and do it again.

	The REPL implies direct interaction with the computer, which seems obvious
	today.  Surprise! This is another innovation that was practically science
	fiction in the 1950s and '60s.  Back then computers were rare and expensive
	enough that every minute of processing time was carefully accounted for.
	"Users" were only allowed to access computers via technicians and often weren't
	allowed to even be in the same room.

	Today, we interact with computers ubiquitously, to a level that we don't even
	call them computers; we call them phones, tablets, watches, cars, and other
	things.  Thank (or curse) Lisp for lighting the path that we took to get here.

	Notice that our REPL might more accurately be called RDorEPL; Read, Define or
	Evaluate-Print, Loop.  To simplify our interpreter, we separate variable
	definition from evaluation.

	*/

	PROC repl

	mov r9, #NIL			/* Start with an empty environment. */

	/* Reset the Lisp registers and Lisp stack. */
10:	ldr r6, =lispstacktop		/* Empty the Lisp stack. */
	mov r8, #NIL			/* The S-expression to evaluate. */
	mov r7, #NIL			/* The value of the evaluated expression. */
	mov r5, #NIL			/* The list of lambda argument values. */

	bl read
	bvs 30f
	mov r8, r7
	bl isdef
	bne 15f
	bl defsym
	bvs 30f
	b 10b
	
15:	bl eval
	bvs 30f
	mov r0, r7
	bl print
	b 10b

	/* Print an error. */
30:	mov r0, r7
	bl prin1
	mov r0, r8
	bl print
	b 10b

	ENDPROC

	/*

	In Lisp, the atom function returns t if its argument is nil or a symbol. It
	returns nil otherwise, which implies that its argument is a cons cell.

	Our assembly version sets the Z condition.
	
	*/
	PROC atom
	push { lr }

	cmp r0, #NIL
	beq 990f
	tst r0, #SYMMASK
	bne 990f

	ZCLR				/* Not an atom. */
	b 999f
	
990:	ZSET				/* We have an atom. */

999:	pop { pc }
	ENDPROC
	
	/*

	Car and cdr are simple in concept, but we implement versions that do some error
	checking.

	Return the car of r0 in r0.  If r0 is nil, return nil.  Otherwise, generate an
	error.

	*/

	PROC car

	push { lr }
	tst r0, #SYMMASK
	beq 1f
	ERRSET errcellornil
	b 999f

1:	cmp r0, #NIL
	beq 999f
	ldr r0, [ r0 ]
	ERRCLR

999:	pop { pc }

	ENDPROC

	PROC cdr

	push { lr }
	tst r0, #SYMMASK
	beq 1f
	ERRSET errcellornil
	b 999f

1:	cmp r0, #NIL
	beq 999f
	ldr r0, [ r0, #4 ]
	ERRCLR

999:	pop { pc }

	ENDPROC

	/*

	A Lisp environment is the current set of variable bindings with which evaluation
	takes place.  At any given moment, the current Lisp environment comprises global
	variables and lambda arguments.  The environment changes as a Lisp program
	directs the interpreter to enter and exit Lisp functions.  We need a data
	structure to handle insertion and deletion of bindings.

	We use an association list, a traditional Lisp data structure.  An association
	list is a list of pairs, in which the car of each pair is a key and the cdr its
	value.  In our case, each key represents a symbol and each value is the value
	bound to the symbol.

	For example, this association list has three pairs that bind a, b, and c to
	abacus, beluga, and cantilever, respectively:

	((a . abacus) (b . beluga) (c . cantilever))

	Given a key, is it bound? If so what is it bound to?

	Here is a good time to introduce recursive functions, which is a classic Lisp
	tool for iteration.  Don't be surprised when I state that Lisp solves iteration
	quite elegantly.

	Here's a common pattern in a Lisp function to iterate over a list:

	* If the list is empty, stop and return a value indicating as much.

	* Otherwise, if the first item in the list is what we're looking for, then
	process it and return.

	* Otherwise, re-apply our function to the rest of the list.

	If the use of the words "first" and "rest" remind you of car and cdr, then
	you're on the right track.

	With that in mind, here's our assoc function:

	(define assoc
	  (lambda (key alist)
	    (cond ((null alist)               nil)
	  	  ((eq key (car (car alist))) (car alist))
		  (t                          (assoc key (cdr alist))))))

	Our assoc function searches for the key parameter in the alist parameter.  The
	body of our assoc function is a single cond expression.  Let's examine each
	clause in the cond:

	The first clause:

	((null alist)               nil)

	This checks to see if our association list is empty.  An empty
	association list means that there are no bindings, so key is obviously not bound
	and we return nil.

	The second clause:
	
	((eq key (car (car alist))) (car alist))

	Is the key equal to the car of the car of the association list?  If they are
	equal, then return the car of the association list, which is the binding pair.

	The last clause:
	
	(t                          (assoc key (cdr alist)))

	This clause acts as a fallback clause, guaranteeing that it will always be
	evaluated when the test expressions of the all other clauses are false.  At this
	point, we have not found the binding we are looking for, so we call assoc again
	with the same key but with the cdr of the association list.  The cdr of the
	association list excludes the car.  Thanks to the second clause, we know that
	the car does not have a binding that matches our key parameter.

	Notice how the last clause calls assoc.  This is the recursive part, which
	re-applies assoc to the next item in the list.  If you're new to recursion, the
	idea takes some getting used to.  Operationally, recursion in this style is the
	same as, say, a for-each loop in conventional languages.  The advantage is that
	you get to concentrate on the problem without the hassle of looping machinery in
	your syntax.

	Now we can implement the assoc function in assembly, specialized for a Lisp
	environment.  For our environment we use the r9 register.  Given a key in r0 and
	an association list (environment) in r9, return the first matching key-value
	pair in r0.  If the key is not in the car of any pair in the list, return nil.

	*/

	PROC assoc

	push { r1-r2, lr }
	mov r1, r0			/* The key to search for. */
	mov r2, r9			/* Association list. */

1:	cmp r2, #NIL			/* At the end of the list? */
	bne 2f
	mov r0, #NIL
	b 999f

2:	mov r0, r2			/* Does the key match this pair? */
	bl car
	bl car
	cmp r1, r0
	bne 3f
	mov r0, r2
	bl car
	b 999f

3:	mov r0, r2			/* No match, try the next pair. */
	bl cdr
	mov r2, r0
	b 1b

999:	pop { r1-r2, pc }

	ENDPROC


	/*

	Determine if the expression pointed to by r8 is a define expression.  If so,
	then set the Z flag, return the symbol to define in r0, and return the
	expression to evaluate in r8.  If not, then clear the status registers.

	*/
	PROC isdef

	push { r1-r2, lr }

	mov r2, r0			/* Save r0. */

	mov r0, r8
	bl atom
	beq 990f
	
	bl car
	ldr r1, =symdefine
	cmp r0, r1
	bne 990f
	
	mov r0, r8			/* Get the symbol to bind. */
	bl cdr
	bvs 990
	bl car
	bvs 990
	mov r1, r0			/* Remember the symbol to bind. */
	
	mov r0, r8			/* Get the expression to evaluate. */
	bl cdr
	bvs 990
	bl cdr
	bvs 990
	bl car
	bvs 990
	bvs 990f
	mov r8, r0
	mov r0, r1
	ZSET				/* We have a define expression. */
	b 999f
	
990:	ZCLR				/* Not a define expression. */
	mov r0, r2			/* Restore r0. */
	
999:	pop { r1-r2, pc }
	ENDPROC
	
	/*

	The define function extends or modifies the environment.  It takes 2 arguments:
	a symbol and an expression to evaluate and bind to the symbol.

	Given a symbol in r0 and an expression in r7, and an environment in r9, evaluate
	the expression then create a binding in the environment.

	In the case of an error, store the offending expression in r8.

	*/

	PROC defsym

	push { r0-r2, lr }

	tst r0, #SYMMASK
	bne 5f
	mov r8, r0
	ERRSET errbindsym
	b 999f

5:	bl eval
	bvs 999f
	
	mov r2, r0			/* Remember our symbol. */

10:	bl assoc
	cmp r0, #NIL
	bne 20f

	/* Symbol is unbound, so bind it. */
	mov r1, r2
	mov r2, r7
	bl cons
	mov r1, r0
	mov r2, r9
	bl cons
	mov r9, r0
	b 990f

	/* Symbol is defined, change its value. */
20:	str r7, [ r0, #4 ]		/* Store it in the cdr. */

990:	ERRCLR
999:	pop { r0-r2, pc }

	ENDPROC


	
	/*

	________________________________________________________________________________

	Evaluation

	We haven't covered what exactly the E means in the REPL.  Evaluation means
	converting an S-expression into a value. 

	Get ready for it: here comes another big Lisp innovation.

	In the paper where he announces Lisp, McCarthy describes the rules for
	evaluating Lisp expressions.  To describe these rules, he presents a definition
	of a Lisp function named eval that evaluates Lisp expressions.  It was a
	ground-breaking feat to define a programming language in terms of itself.  It
	was an obvious thing to do, considering Lisp's expressiveness.  It was also
	remarkably concise. 

	Computer scientists call this "self-interpreting".  A self-interpreting language
	is astonishingly easy to extend.  Just modify its interpreter, which is the
	definition of the language, which is the interpeter...

	Steve Russell gets credit for implementing the seminal eval in IBM 704 assembly
	language, paving the way for a real, honest to greatness Lisp interpreter.
	Thanks to him, it was possible to interact with Lisp directly just by
	running a Lisp program.

	To implement our own eval, we follow McCarthy and Russell's lead by first
	defining it in Lisp before implementing it in assembly.

	We start with the simplest of Lisp expressions: () and t.  These are known as
	self-evaluating expressions because they evaluate to themselves:

	(define eval
	  (lambda (expr)
	    (cond
	     ((selfevalp expr)      expr)
	     (t                     (quote errbadlistexpr)))))

	The selfevalp function returns true if an expression is self-evaluating, false 
	otherwise.

	(define selfevalp
	  (lambda (x)
	    (cond ((null x)           t)
		  ((eq x (quote t))   t)
		  (t                  nil))))

	Side note: In Lisp, functions that return a true or false value are
	traditionally called predicates.  A predicate often has the letter "p" appended
	to its name. The null and atom functions are historical exceptions.

	Calling our nascent eval with () or t returns each of these, respectively.
	If it fails to recognize an expression, it returns errbadlistexpr.
	Examples:

	(eval (quote ()))
	nil

	(eval (quote t))
	t

	(eval (quote name))
	errbadlistexpr

	Next, we add nil:

	(define eval
	  (lambda (expr)
	    (cond
	     ((selfevalp expr)      expr)
	     ((eq expr (quote nil)) ())
	     (t                     (quote errbadlistexpr)))))
	
	(eval (quote nil))
	nil

	The next simplest expression is a symbol, which evaluates to the value it is
	bound to.  To extend our eval to handle symbols, we introduce the env parameter
	to specify bindings:

	(define eval
	  (lambda (expr env)
	    (cond
	     ((selfevalp expr)      expr)
	     ((eq expr (quote nil)) ())
	     ((symbolp expr)        (cdr (assoc expr env)))
	     (t                     (quote errbadlistexpr)))))

	The symbolp predicate returns true if its argument is a symbol, false
	otherwise:
	
	(define symbolp
	  (lambda (x)
	    (cond ((atom x) (cond ((null x) nil) (t t)))
		  (t        nil))))

	Calling our new eval with an environment argument gives us access to variable
	bindings:

	(define e (quote ((B . (beluga)) (C . cantilever))))
	(eval (quote B) e)
	(beluga)

	(eval (quote C) e)
	cantilever

	To evaluate functions, we extend our eval to handle list expressions.  At this
	point we can safely assume that an expression is a list because previous cond
	clauses have already checked for all possible non-list expressions.

	When the car of a list expression is a symbol for a built-in function or special
	form, evaluate the rest of the list expression accordingly.  We start with a
	simple list expression, quote:

	(define eval
	  (lambda (expr env)
	    (cond
	     ((selfevalp expr)               expr)
	     ((eq expr (quote nil))          ())
	     ((symbolp expr)                 (cdr (assoc expr env)))
     	     ((eq (car expr) (quote quote))  (arg1 expr))
	     (t                              (quote errbadlistexpr)))))

	The arg1 and arg2 functions return the first and second arguments of a list
	expression, respectively:

	(define arg1 (lambda (expr) (car (cdr expr))))
	(define arg2 (lambda (expr) (car (cdr (cdr expr)))))

	Other special forms and functions follow a similar pattern as quote.  Here's the
	complete eval:
	
	(define eval
	  (lambda (expr env)
	    (cond
	     ((selfevalp expr)               expr)
	     ((eq expr (quote nil))          ())
	     ((symbolp expr)                 (cdr (assoc expr env)))
     	     ((eq (car expr) (quote quote))  (arg1 expr))
	     ((eq (car expr) (quote lambda)) expr)
	     ((eq (car expr) (quote atom))   (atom (eval (arg1 expr) env)))
	     ((eq (car expr) (quote car))    (car (eval (arg1 expr) env)))
	     ((eq (car expr) (quote cdr))    (cdr (eval (arg1 expr) env)))
	     ((eq (car expr) (quote eq))     (eq
					      (eval (arg1 expr) env)
					      (eval (arg2 expr) env)))
	     ((eq (car expr) (quote cons))   (cons
					      (eval (arg1 expr) env)
					      (eval (arg2 expr) env)))
	     ((eq (car expr) (quote cond))   (evalcond (cdr expr) env))
	     (t                              (apply
	                                      (eval (car expr) env)
	                                      (evlis (cdr expr) env) env)))))

	This is our biggest Lisp function, so take your time to look it over.  Notice
	that most built-in functions, like atom, evaluate their arguments by calling
	eval.

	Also notice that a lambda expression, unlike quote, returns itself entirely.
	This is related to the last clause, which calls the apply function.  The apply
	function handles user-defined functions.  We cover it in detail later.

	Side note: Not only do we define eval in terms of itself but we conveniently
	ignore implementing built-in functions and special forms by using the same
	built-in functions and special forms that Lisp provides.  This chicken-and-egg
	situation is often called "snarfing" in Lisp.  It might be confusing, but it's
	ok here.  Remember that we are specifying our Lisp interpreter in Lisp rather
	than defining a working version of it.  The actual implementation is in
	assembly, which gets us outside the chicken-and-egg loop.

	Another side note: Our Lisp definition of eval is an actual, working Lisp
	interpreter.  As an exercise, enter this eval and its helper functions in the
	arpilisp intrepreter.  Make arpilisp interpret itself!

	Before eval, let's define some helper functions and macros.

	*/

	.macro MATCHFORM sym
	mov r0, r8
	bl car
	ldr r1, =\sym
	cmp r0, r1
	.endm

	.macro ARG1
	mov r0, r8
	bl cdr
	blvc car
	.endm

	.macro ARG2
	mov r0, r8
	bl cdr
	blvc cdr
	blvc car
	.endm
	
	PROC selfevalp
	push { r1, lr }

	cmp r0, #NIL
	beq 999f

	ldr r1, =symt
	cmp r0, r1
	
999:	pop { r1, pc }
	ENDPROC
	
	/*

	Our ARM assembly eval takes an expression in r8 and an environment in r9,
	returning a value in r7.

	When we encounter an error, we try to leave the offending expression in r8.
	That means we don't pop r8 from the Lisp stack.  We can get away with this
	imbalanced push-pop because our REPL resets the Lisp stack for us.

	*/

	PROC eval
	push { r0-r2, lr }

	mov r0, r5
	bl pushlisp
	bvs 999f
	mov r0, r8
	bl pushlisp
	bvs 999f

	// ((selfevalp expr) expr)
	bl selfevalp
	bne 10f
	mov r7, r8
	b 990f

	// ((eq expr (quote nil)) ())
10:	ldr r1, =symnil
	cmp r0, r1
	bne 20f
	mov r7, #NIL
	b 990f

	// ((symbolp expr) (cdr (assoc expr env)))
20:	tst r0, #SYMMASK
	beq 40f
	bl assoc
	cmp r0, #NIL
	bne 30f
	ERRSET errunboundvar
	b 999f
	
30:	bl cdr
	bvs 999f
	mov r7, r0
	b 990f

40:	// ((eq (car expr) (quote quote)) (arg1 expr))
	MATCHFORM symquote
	bne 50f
	ARG1
	bvs 999f
	mov r7, r0
	b 990f

50:	// ((eq (car expr) (quote lambda)) expr)
	MATCHFORM symlambda
	bne 60f
	mov r7, r8
	b 990f

	// ((eq (car expr) (quote atom))   (atom (eval (arg1 expr) env)))
60:	MATCHFORM symatom
	bne 70f
	ARG1
	bvs 999f
	mov r8, r0
	bl eval
	bvs 999f
	mov r0, r7
	bl atom
	ldreq r7, =symt
	movne r7, #NIL
	b 990f

	// ((eq (car expr) (quote car))    (car (eval (arg1 expr) env)))
70:	MATCHFORM symcar
	bne 80f
	ARG1
	bvs 999f
	mov r8, r0
	bl eval
	bvs 999f
	mov r0, r7
	bl car
	bvs 999f
	mov r7, r0
	b 990f
	
	// ((eq (car expr) (quote cdr))    (cdr (eval (arg1 expr) env)))
80:	MATCHFORM symcdr
	bne 90f
	ARG1
	bvs 999f
	mov r8, r0
	bl eval
	bvs 999f
	mov r0, r7
	bl cdr
	bvs 999f
	mov r7, r0
	b 990f

	// ((eq (car expr) (quote eq))     (eq
	//				      (eval (arg1 expr) env)
	//				      (eval (arg2 expr) env)))
90:	MATCHFORM symeq
	bne 100f
	ARG1
	bvs 999f
	mov r1, r0			/* Unevaluated 1st argument. */
	ARG2
	bvs 999f
	mov r2, r0			/* Unevaluated 2nd argument. */
	mov r8, r1			/* Evaluate 1st argument. */
	bl eval
	bvs 999f
	mov r1, r7
	mov r8, r2			/* Evaluate 2nd argument. */
	bl eval
	bvs 999f
	mov r2, r7
	cmp r1, r2
	movne r7, #NIL
	ldreq r7, =symt
	b 990f

	// ((eq (car expr) (quote cons))     (cons
	//				      (eval (arg1 expr) env)
	//				      (eval (arg2 expr) env)))
100:	MATCHFORM symcons
	bne 110f
	ARG1
	bvs 999f
	mov r1, r0			/* Unevaluated 1st argument. */
	ARG2
	bvs 999f
	mov r2, r0			/* Unevaluated 2nd argument. */
	mov r8, r1			/* Evaluate 1st argument. */
	bl eval
	bvs 999f
	mov r1, r7
	mov r8, r2			/* Evaluate 2nd argument. */
	bl eval
	bvs 999f
	mov r2, r7
	bl cons
	mov r7, r0
	b 990f

	// ((eq (car expr) (quote cond))   (evalcond (cdr expr) env))
110:	MATCHFORM symcond
	bne 120f
	mov r0, r8
	bl cdr
	bvs 999f
	bl evalcond
	bvs 999f
	b 990f

	// (t (apply (eval (car expr) env) (evlis (cdr expr) env) env)))))
120:	mov r0, r8
	bl car
	bvs 999f
	mov r1, r0			/* Unevaluated function. */

	mov r0, r8
	bl cdr
	bvs 999f
	mov r2, r0			/* Unevaluated arguments. */

	mov r8, r1
	bl eval
	bvs 999f
	mov r1, r7			/* Evaluated function. */

	mov r0, r2
	bl evlis
	bvs 999f
	
	mov r0, r1
	bl apply
	bvs 999f

990:	bl poplisp
	mov r8, r0
	bl poplisp
	mov r5, r0
	ERRCLR
	
999:	pop { r0-r2, pc }
	ENDPROC

	/*

	More helper functions.

	First, let's implement cond.  This assembly procedure implements this Lisp
	function:

	(define evalcond (lambda (c env)
	  (cond ((eq c nil) nil)
		((eval (car (car c)) env) (eval (car (cdr (car c))) env))
		(t (evalcond (cdr c) env)))))

	Given a list of cond clauses in r0, return the value of the first clause that
	tests true in r7.

	*/

	PROC evalcond

	push { r0-r1, r8, lr }

	mov r1, r0

	// ((eq c nil) nil)
10:	cmp r0, #NIL
	moveq r7, r0
	beq 990f

	// ((eval (caar c) env) ...
	bl car
	blvc car
	mov r8, r0
	bl eval
	bvs 999f
	cmp r7, #NIL
	beq 20f

	// ... (eval (cadar c) env))
	mov r0, r1
	bl car
	blvc cdr
	blvc car
	bvs 999f
	mov r8, r0
	bl eval
	bvs 999f
	b 990f

	// (t (evalcond (cdr c) env))))
20:	mov r0, r1
	bl cdr
	bvs 999f
	mov r1, r0
	b 10b


990:	ERRCLR
999:	pop { r0-r1, r8, pc }

	ENDPROC

	/*

	Before applying a Lisp function, we need to evaluate the arguments we are
	passing to it.

	The Lisp equivalent:

	(define evlis
	  (lambda (exprs env)
	    (cond ((null exprs) nil)
	          (t (cons
	               (eval (car exprs) env)
	               (evlis (cdr exprs) env))))))

	Given a list of unevaluated expressions in r0 and an environment pointed to by
	r9, return a corresponding list of evaluated values in r5. The evaluated values
	are in the same order as the values in the unevaluated list.

	*/

	PROC evlis

	push { r0-r4, r8, lr }

	mov r5, #NIL			/* Head of the values list. */
	mov r3, #NIL			/* End of the values list. */
	mov r4, r0			/* Remaining unevaluated expressions. */

	cmp r0, #NIL			/* Any expressions? */
	beq 990f

	bl car				/* Evaluate the first expression. */
	mov r8, r0
	bl eval
	bvs 999f

	mov r1, r7 			/* Start our list of values. */
	mov r2, #NIL
	bl cons
	mov r5, r0
	mov r3, r0
	mov r0, r4
	bl cdr
	mov r4, r0

10:	cmp r0, #NIL			/* Any more expressions? */
	beq 990f

	bl car
	bvs 999f
	mov r8, r0
	bl eval
	bvs 999f

	mov r1, r7
	mov r2, #NIL
	bl cons
	str r0, [ r3, #4 ]
	mov r3, r0

	mov r0, r4
	bl cdr
	mov r4, r0

	b 10b

990:	ERRCLR
999:	pop { r0-r4, r8, pc }

	ENDPROC

	/*

	The other half of evaluation is the apply function.  Apply executes a lambda
	function by evaluating each expression in a lambda body, in order, and returns
	the value of the last expression.

	Our assembly apply procedure implements these Lisp functions:

	(define apply
	  (lambda (fn args env)
	    (cond
	      ((null (cdr (cdr fn))) nil)
	      (t (apply-body (cdr (cdr fn)) (pairlis (car (cdr fn)) args env))))))
	
	(define apply-body
	  (lambda (body env)
	    (cond
	      ((null (cdr body)) (eval (car body) env))
	      (t (apply-next body env)))))

	(define apply-next
	  (lambda (body env)
	    (eval (car body) env)
	    (apply-body (cdr body) env)))

	Ironically, the Lisp definition for apply seems more complicated than its
	assembler equivalent below.  This difference is mostly due to Lisp's coziness to
	functional purity.  That's another way of saying, for our purposes anyway, that
	Lisp encourages us to avoid storing things in global variables.

	Assembly language offers almost nothing beyond the ability to store things in
	global variables.  This happens to be beneficial here, letting us take a more
	direct route to implement the logic of apply.  In our case, we have r7, which
	contains the value of the most recently evaluated expression.

	For what it's worth, a more sophisticated Lisp would allow us to define a much
	shorter, single-function apply.

	Given a lambda in r0, a list of values in r5, and an environment in r9, bind
	each lambda argument to its value then evaluate each of the expressions in the
	lambda body, returning the value of the last expression in r7.  If the body is
	empty, return nil.

	We need to be careful about r9, our environment.  If there's an error during the
	evaluation of the lambda body, we need to restore r9 to the pre-apply
	environment.

	*/

	PROC apply

	push { r0-r2, r8-r9, lr }

	mov r2, r0			/* Remember our function. */

	/* Make sure we have a lambda. */
	bl car
	bvs 980f
	
	ldr r1, =symlambda
	cmp r0, r1
	bne 980f

	mov r7, #NIL			/* Assume an empty body. */

	mov r0, r2
	bl cdr				/* Skip the lambda symbol. */

	mov r2, r0			/* Bind the parameters. */
	bl car
	bl pairlis
	bvs 999f

	mov r0, r2			/* Skip the parameter list. */
	bl cdr
	mov r2, r0

	/* Apply the body. */
10:	cmp r0, #NIL			/* Any more expressions? */
	beq 990f
	bl car				/* Evaluate the next expression. */
	bvs 999f
	mov r8, r0
	bl eval
	bvs 999f			/* Give up if there's an error. */
	mov r0, r2
	bl cdr
	mov r2, r0
	b 10b

980:	ERRSET errbadlistexpr
	b 999f

990:	ERRCLR
999:	pop { r0-r2, r8-r9, pc }

	ENDPROC

	/*

	We need a way to bind a lambda's parameters to values then extend the
	environment with these bindings.  As we saw earlier, lambda parameters are
	independent of existing bindings of the same symbols.

	The arguments for a lambda application are evaluated in the environment outside
	of the lambda.

	For example:

	(define x (quote marks-the-spot))

	((lambda (x) (null x)) x)

	nil

	Redefining x gives a different result:

	(define x nil)

	((lambda (x) (null x)) x)
	t

	A lambda can also use a binding that exists before the lambda is applied.  For
	example:

	(define name (quote Nicole))

	((lambda (x) (eq name x)) (quote Nicole))
	t

	For arpilisp, we implement this:

	(define pairlis
	  (lambda (params vals env)
	    (cond
	      ((null params) (cond ((null vals) env) (t (quote errargextra))))
	      ((null vals) (quote errargmissing))
	      (t (cons
	           (cons (car params) (car vals))
	           (pairlis (cdr params) (cdr vals) env))))))
  
	Given a list of parameters in r0, a list of values in r5, and an environment to
	extend in r9, bind each parameter to a value then extend r9.

	*/

	PROC pairlis

	push { r0-r3, lr }

	mov r3, r0			/* List of parameters. */

5:	cmp r3, #NIL			/* Are parameters and values empty? */
	bne 20f
	cmp r5, #NIL
	beq 990f

10:	cmp r3, #NIL
	bne 20f
	ERRSET errargextra
	b 999f

20:	cmp r5, #NIL
	bne 30f
	ERRSET errargmissing
	b 999f

30:	mov r0, r3
	bl car
	mov r1, r0
	mov r0, r5
	bl car
	mov r2, r0
	bl cons
	mov r1, r0
	mov r2, r9
	bl cons
	mov r9, r0
	mov r0, r3
	bl cdr
	mov r3, r0
	mov r0, r5
	bl cdr

	mov r5, r0
	b 5b

990:	ERRCLR
999:	pop { r0-r3, pc }

	ENDPROC

	/*
	________________________________________________________________________________

	The End

	And the beginning.  Arpilisp's eval provides the core of a Lisp interpreter.  You
	can use arpilisp to implement the rest of a pretty capable Lisp system.

	To start, you can add some traditional utility functions like caar, cadr,
	cdar, cddr.  You can add some powerful function application functions like
	map.  And of course, you can extend eval by writing your own.

	If you store the rest of your Lisp in a file named my.lisp, you can load
	it and continue in the REPL with this shell command:

	cat my.lisp - | ./arpilisp

	Enjoy yourself.

	________________________________________________________________________________

	MIT License

	Copyright (c) 2016 Marc Paquette

	Permission is hereby granted, free of charge, to any person obtaining a copy
	of this software and associated documentation files (the "Software"), to deal
	in the Software without restriction, including without limitation the rights
	to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
	copies of the Software, and to permit persons to whom the Software is
	furnished to do so, subject to the following conditions:

	The above copyright notice and this permission notice shall be included in all
	copies or substantial portions of the Software.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
	AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
	LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
	SOFTWARE.
	
	*/
	
