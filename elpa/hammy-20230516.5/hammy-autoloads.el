;;; hammy-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "hammy" "hammy.el" (0 0 0 0))
;;; Generated autoloads from hammy.el

(autoload 'hammy-define "hammy" "\
Define a new Hammy named NAME made with ARGS.
Returns the hammy, and adds hammy to `hammy-hammys'.  NAME is a
string.  ARGS are passed to `make-hammy', which see.  Useful ones
include:

  `:documentation': An optional documentation string.

  `:intervals': A list of intervals.  Each one is defined with
    the local function `interval', which calls
    `make-hammy-interval', which see for its arguments.

  `:before': One or a list of functions which are called when the
    interval begins.  See the `do' macro, documented later.

  `:after': One or a list of functions which are called when the
    interval ends.  See the `do' macro, documented later.

  `:duration': A number of seconds, or a string passed to
    `timer-duration' to return such, or a function which returns
    such (called before starting the interval each cycle).  See
    the `do' macro, documented later.

  `:advance': Nil to advance automatically, or one or a list of
    functions to call when the interval's timer has elapsed and
    the user should be prompted to manually advance to the next
    interval.  See the `do' and `remind' macros, documented
    later.

Within ARGS, these pseudo-functions and forms available:

  `announce (message)': Announce MESSAGE in the echo area.
  `notify (message)`: Send MESSAGE as a desktop notification.

  `climb (from to &key descend step)': Return a function that
    returns a duration gradually increasing from FROM to TO, and
    optionally decreasing back to TO, by STEP.  FROM, TO, and
    STEP may be numbers or strings (passed to `timer-duration',
    which see).  DESCEND, if non-nil, causes the duration to
    gradually decrease back to FROM after reaching TO.

  `duration (interval)': Return a number of seconds equivalent to
    INTERVAL (a string like \"10 minutes\").  Calls
    `timer-duration', which see.

  `do (&rest body)': Expands to a lambda that binds `hammy' to
    the current hammy and evaluates BODY.  Within its BODY, these
    forms are bound:

    `current-duration': The duration in seconds of the current interval.
    `current-interval-start-time': The time at which the current interval began.
    `cycles': The number of cycles the hammy has completed.
    `etc': The hammy's `etc' slot.
    `history': The hammy's history list.
    `interval': The current interval (a `hammy-interval' struct).
    `interval-name': The name of the current interval.

  `elapsed (&optional interval)': Calls `hammy-elapsed' with the
    hammy, which see.

  `interval (&rest args)': Calls `make-hammy-interval', which
    see.

  `num-intervals ()': Returns the hammy's number of intervals.

  `remind (delay &rest fns)': Return a function that is called
    every DELAY seconds until the interval is manually advanced,
    calling FNS each time.  (The function automatically makes
    necessary adjustments to the hammy to set and cancel the
    periodic reminders.)

  `run (command)': Runs COMMAND (a string) asynchronously with
    `make-process', discarding its output and return value.

\(fn NAME &rest ARGS)" nil t)

(function-put 'hammy-define 'lisp-indent-function 'defun)

(autoload 'hammy-start "hammy" "\
Start HAMMY and return it.
If DURATION, set its first interval to last that many seconds.
INTERVAL may be an interval in the hammy to start
with (interactively, with universal prefix, prompt for the
interval with completion).

\(fn HAMMY &key DURATION INTERVAL)" t nil)

(autoload 'hammy-start-org-clock-in "hammy" "\
Call `org-clock-in' and start a hammy (or use an already-started one).
If point is in an Org entry, clock into it; otherwise, offer a
list of recently clocked tasks to clock into.  The Org task will
then automatically be clocked out during the hammy's second
interval (and when the hammy is stopped), and back in when the
first interval resumes.  (If the user clocks into a different
task while the hammy is running, the task that is clocked-in when
the work interval ends will be clocked back into when the next
work interval begins.)

Returns the hammy from `hammy-start'.  Assumes that the hammy's
first interval is the work interval (i.e. the one during which
the task should be clocked in).

\(fn &rest IGNORE)" t nil)

(defvar hammy-mode nil "\
Non-nil if Hammy mode is enabled.
See the `hammy-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `hammy-mode'.")

(custom-autoload 'hammy-mode "hammy" nil)

(autoload 'hammy-mode "hammy" "\
Show active hammy in the mode line.

This is a minor mode.  If called interactively, toggle the `Hammy
mode' mode.  If the prefix argument is positive, enable the mode,
and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='hammy-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "hammy" '("hammy-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; hammy-autoloads.el ends here
