
CONTENTS

 . CONTENTS
 . README
 . INSTALLATION
 . CONFIGURATION
    . debug
    . default frame manager
    . listener
 . KNOWN LIMITATIONS / TODO LIST
 . WISH LIST
 . APPLICATIONS

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

README

This document contains my ongoing installation and execution instructions
for the McCLIM 'Beagle' back end, along with a list of existing limitations
known within the system.

These limitations form an effective TODO list so I also added my wish list
into this file so a good overview of the current state and future direction
of the code can be provided, hopefully.

As usual in these situations, I must take responsibility for all poor bits
of code / bugs (in actual fact in this case it's really true; I'm not very
experienced and have made many silly decisions, probably.)

-Duncan
duncan@robotcat.demon.co.uk

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

INSTALLATION

The code has been written using OpenMCL Version (Beta: Darwin) 0.14.1-p1 and
up-to-date McCLIM sources (since both are available within the same CVS
module, it should be safe to assume the back end will work with whatever
McCLIM sources were checked out at the same time). Hopefully newer versions
of OpenMCL will be ok; unfortunately older versions will not work due to
changes in the OpenMCL Cocoa Bridge.

Compiling and running the back end currently is a straight-forward (if
rather limiting [see note #3]) task:

Optional:

1.  Create a symbolic link from .../McCLIM/Backends/beagle/load-clim.lisp
    to your home directory.
2.  Ditto for load-clx.lisp (if you intend to run the clx backend too... there
    are other ways to do this, but this is how I'm doing it at the moment)
3.  Ditto for load-beagle.lisp

Then:

2.  Start OpenMCL
3.  Evaluate '(require "COCOA")'

The following are evaluated from the 'OpenMCL Listener' that opens:

4.  Evaluate '(require "ASDF")'
5.  Evaluate '(load "home:load-clim")' [See note #1]
6.  Evaluate '(load "home:load-beagle")' [See note #4]

The McCLIM Listener should now be able to be started from the OpenMCL
Listener by evaluating '(clim-listener:run-listener)'. See the McCLIM
installation notes for other things you might want to do. [See note #2]

Note #1: If you did not create the symbolic link in (1), load the
         "load-clim.lisp" file from whereever it is; for example,

         '(load "/Users/me/McCLIM/Backends/beagle/load-clim")'

Note #2: Some of the examples provided with McCLIM do not execute when using
         the Beagle back end, either because of unimplemented features in
         the back end or because of lossage in the back end. Reports and
         patches would be appreciated!

Note #3: Yes, this is a little silly. For a while the Beagle back end could
         be built into its own bundle by making use of Mikel Evins'
         Bosco framework. This framework is currently undergoing some change
         in any case so I decided to make the initial CVS version available
         without providing this nicety. Hopefully in the future (yes, this
         is a TODO!) this facility will be made available again.
         Another alternative would be to incorporate the same hacks that the
         OpenMCL Cocoa examples use in order to convince Cocoa that the
         currently-executing (non-graphical) application can have an event
         loop.

Note #4: If you'd rather run with the CLX back end, do a load-clx instead
         here. Hopefully it will (soon?) be possible to run with multiple
         ports simultaneously so that both a CLX and a Beagle Listener can
         be run side by side for comparative purposes.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

CONFIGURATION

debug:
------
Most debug output within the Beagle back end uses a specialized debug
logging method so it can be dynamically turned on and off. If for any
reason you want to observe log messages, set the following parameter to a
non-zero integer (the higher the integer, the more detail there is to the
logging. No logging is higher than 4 at the moment though I don't think).

CL-USER:*DEBUG-LOG-LEVEL* <defined in 'package.lisp'>
  -> numeric  [0 by default]

See also TODO item 12. In general I need to go through all the debug
messages and sort them out.


default frame manager:
----------------------
The Beagle back end defines two frame manager objects; one is the aqua
look and feel, the other is the 'standard' McCLIM look and feel. If you
want to configure a specific frame manager to be used, set the following
parameter:-

BEAGLE::*DEFAULT-BEAGLE-FRAME-MANAGER* <defined in 'port.lisp'>
  -> 'beagle::beagle-aqua-frame-manager  [default]
  -> 'beagle::beagle-standard-frame-manager

Should use CLIM:*DEFAULT-FRAME-MANAGER* for this!
Note that as yet, no native (aqua) look and feel panes have been defined,
so it doesn't matter which one you use.


listener:
---------
If you want to run the Listener in this back end as it currently stands, you
need to make the following modifications to
'Apps/Listener/dev-commands.lisp':-

1. Modify 'pretty-pretty-pathname', removing:

   (let ((icon (icon-of pathname)))
     (when icon (draw-icon stream icon :extra-spacing 3)))

2. Modify 'com-show-directory', removing:

   (draw-icon T (standard-icon "up-folder.xpm") :extra-spacing 3)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

KNOWN LIMITATIONS / TODO LIST

1.  Speed! The current implementation is __slow__, especially when there is a
    large output history. Paolo's speed test takes 26 seconds and conses
    16MB on my (admittedly slow) iMac compared to 1.5 seconds on a 2.4GHz
    Pentium IV and unknown (to me) consing.
    Should be able to speed things up by performing fewer focus lock / unlocks,
    and by not setting drawing options unless necessary. I don't know how
    far this will get us though...

2.  When running the Listener (and probably other applications), the resize
    handle is not visible; it's there, but you can't see it. Grab and drag
    with faith and it should work anyway.

3.  There are not yet any aqua look and feel panes. Sorry, I'm trying to
    get everything else working first!

-4.-  Pixmap support is not implemented; this means clim-fig drawing doesn't
    work.
    This is getting there, although not very efficiently; we are missing a
    method for (sheet-mirror mirrored-pixmap). This is evident if you run
    (clim-demo::clim-fig) and actually do some drawing.
    RESOLVED 08.AUG.04 [NB. this functionality is not too efficient I think
                       and needs revisiting (like everything else does)]

4.5. Designs (other than colours) aren't implemented - THIS means there are
    no icons in the Listener.

5.  Mouse down / up on buttons appears not to work very well unless the frame
    containing the buttons is the only active frame.
    Actually, this ^^^ seems to work fine, but the highlighting for button
    gadgets looks screwy under OS X.

6.  Swapping between key windows (the window accepting the keyboard input)
    is a little flakey; as an example, if a second Listener is started from
    the first, clicking between the windows transfers key focus (as
    expected). However, if the first is then 'Exit'ed, the second will not
    get the key focus until some other (non-McCLIM) window has been given
    the keyboard focus first (i.e. click on the OpenMCL Listener window,
    then back on the McCLIM Listener window).
    Additionally, clicking on a scroll-bar (for example) makes the window
    key, so clicking on a view that accepts keyboard input (interactor)
    won't then allow keyboard input.
    We should stop scroll-bars being able to get keyboard input...

7.  Keyboard events are not handled "properly" as far as any OS X user will
    be concerned; only the ASCII characters are recognised, along with
    simple modifiers. It's enough to enter commands and edit the command via
    Emacs-like CTRL-B (backward char), CTRL-F (forward char), CTRL-A (start
    of line), CTRL-E (end of line), CTRL-D (delete char).

8.  There is no +flipping-ink+ implementation. Anything drawn in flipping
    ink shows up bright red with about 50% transparency (which is why the
    cursor looks so strange).

9.  Foreign memory management is non-existent. This is fine whilst running
    in the same thread as the OpenMCL Listener (since we use its autorelease
    pool) but when running in a separate thread lots of warning messages
    are generated.

10. Text sizes aren't calculated correctly; when multiple lines are output
    together, the bottom of one line can be overwritten by the top of the
    next line.

11. Line dash patterns haven't been implemented.

12. There's probably some debug output remaining in some corner cases.

-13.- Some Apropos cases fail; for example 'Apropos graft' fails (although
    '(apropos 'graft)' does not). The same problem prevents the address
    book demo working too I think. [This appears to be caused by treating
    any 'NIL' which should be output as a literal object. Not sure how
    this is happening, but it should be possible to track it down].
    RESOLVED 21.AUG.2004 - it appears MACPTRs are output using the family
    (for a text style) of :fixed - which didn't exist (only :fix). Not
    sure if this is a specification violation or not...

-14.- Not all foreign objects we keep hold of in the back end are heap-
    allocated. Some are stack-allocated and cause errors about 'bogus'
    objects once they go out of scope. At least, I think (and hope) that's
    the reason 'cause that's easy to fix. RESOLVED 17.JUL.04

15. Popup menus don't work quite the same way as they do in the CLX back
    end. Cocoa doesn't support pointer grabbing so disposing of menus when
    the mouse pointer moves off them doesn't work in Beagle (either choose
    a command, or mouse-click on a different window to get rid of them).
    Additionally, highlighting the menu item the mouse is currently over
    is rather intermittent, although the correct menu item appears to always
    be chosen on mouse-click.

16. Windows are put on screen very early in the realization process which
    wasn't a bad thing during early development (could see how far through
    things got before blowing up) but now it just looks messy.

17. *BEAGLE-DEFAULT-FRAME-MANAGER* should be replaced with the standard
    *DEFAULT-FRAME-MANAGER* instead.

18. The back end doesn't clear up after itself very well. You might find it
    necessary to force-quit OpenMCL after you've finished.

19. Menus don't work in CLIM-FIG (or any else!). No idea why not...
    This is because the way pointer tracking is done in clim-internals has
    been changed, so another work-around needs to be implemented.

20. Bounding rectangles are slightly off (this can be seen in CLIM-FIG again).
    It's only a matter of a pixel, maybe 2 in the worst case I've seen.
    Probably caused by rounding errors in Beagle (we do quite a lot of
    int -> float conversion and ordinate manipulation (in cocoa 0.0, 0.0 falls
    'between pixels' - 0.5, 0.5 is 'center of pixel').

21. Highlighting on mouse overs isn't quite right; artefacts are left on the
    display after the mouse has moved out of the target object bounding
    rectangle (most easily visible in CLIM-FIG again).

22. Sending key-down / key-up events for modifiers-changed events doesn't
    look to help get the pointer documentation pane to show the correct
    prompt. For example, in the Listener, issue a 'help commands' and
    mouse over one of the elements. The PDP says 'R: Menu / To see other
    commands, press Super'. If super is pressed, the highlighting on the
    presentation is lost, and the PDP is blanked out. Only when the mouse
    moves again is the PDP redisplayed correctly, with 's-L: Describe
    Presentation'.
    Need to check CLX implementation to see if this is the same...

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

WISH LIST

1.  Bring Beagle back end into line with CLX back end in terms of supported
    McCLIM functionality (basically - -pixmap-support-,- flipping ink and line
    dashes)

2.  Implement native look and feel

3.  Integrate the OpenMCL break-loop into the McCLIM Listener; at the moment
    you end up dumped either in the OpenMCL Listener or the terminal window,
    depending (on what I haven't quite worked out) - generally the former.

4.  Look again at the build process and reintegrate with Bosco.

5.  Possibly migrate to being a Carbon, rather then a Cocoa, application to
    remove OpenMCL version dependencies.

6.  Reduce focus locking in NSViews (I think this will give a not
    insignificant speed increase).

7.  Documentation

8.  Code tidying, and lots of it! Refactoring. Need to implement many
    abstractions (which should also help in the Cocoa -> Carbon move,
    when it happens).

9.  Release resources on exit.

10. Test / fix multiple port, multiple screen, multiple frame managers.

11. Write general test suite for McCLIM in general and Beagle back end in
    particular.

12. Look again at sheet hierarchy stuff; I'm pretty sure this only works
    when the graft is in the default orientation.

13. I'd like to see the silica functionality in a separate package; I
    think (need to check!) that silica + back-end implementation should
    permit a 'stupid' (i.e. not presentation based) GUI to be written.
    Some people might find this useful.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

APPLICATIONS

One day, I'd actually like to use McCLIM to write some applications ;-)
The following are on the wish-list:

1.  Editor, one of this list most likely:
      + Goatee extensions
      + DUECE port
      + portable hemlock

2.  Inspector

3.  Call tracer / grapher

4.  Debugger (incl. breakpoints / stepping / inspector integration etc.)

5.  Documentation tool

6.  News reader (seems likely Hermes can be ported from CLIM 1 / Genera ->
    McCLIM)

7.  Email client

8.  Hyperspec viewer / WWW browser (Closure browser?)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
