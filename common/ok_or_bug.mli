(** Strip down an Or_error.t that is *always* Ok, failing loudly if we're wrong.

    Essentially identical to Or_error.ok_exn, with a louder failure.  The problem with
    [Or_error.ok_exn] is that it raises an exception on a non-Ok value, which might be an
    expected, handled exceptional situation in the correct functioning of the program, or
    it might be a bug in the program.  *This* function should be used in contexts where a
    non-Ok value is a *bug* in the program -- handling a non-Ok value means changing the
    source code.

    One could argue that this function should call exit(1) -- the point is that a bug has
    been discovered. Stop running the program! This is probably a little extreme -- we
    might want to *try* and unwind out of our current context, shut things down in a
    controlled way.

    However, because we'd like to keep it a little undefined what the effects (control and
    otherwise) are when this check fails, we leave _exn off the name of the function. *)

open! Core

val ok_or_bug : 'a Or_error.t -> 'a
