
CONTENTS

 . CONTENTS
 . README
 . INSTALLATION
 . CONFIGURATION
    . frame manager
    . multiple ports
 . KNOWN LIMITATIONS / TODO LIST
 . NATIVE PANES
 . FIXED STUFF PREVIOUSLY ON THE KNOWN LIMITATIONS / TODO LIST
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

I've taken a look at the Beagle backend for the .9.2 release of McCLIM
and added some of my own notes and edits to this file.

-Tim
moore@bricoworks.com

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

INSTALLATION

The code has been tested using OpenMCL Version 1.0 and up-to-date
McCLIM sources (since both are available within the same CVS module,
it should be safe to assume the back end will work with whatever
McCLIM sources were checked out at the same time). Hopefully newer
versions of OpenMCL will be ok; unfortunately older versions will not
work due to changes in the OpenMCL Cocoa Bridge.

Compiling and running the back end currently is a straight-forward (if
rather limiting [see note #2]) task:

1.  Install McCLIM according to INSTALL.ASDF in the McCLIM root
    directory.
2.  Start OpenMCL
3.  Evaluate '(require :cocoa)'

The following are evaluated from the 'OpenMCL Listener' that opens:

4.  Evaluate '(asdf:oos 'asdf:load-op :clim-beagle)'
5.  Evaluate '(asdf:oos 'asdf:load-op :mcclim)' [See note #3]

The McCLIM Listener should now be able to be started from the OpenMCL
Listener by evaluating '(clim-listener:run-listener)' after loading it
with '(asdf:oos 'asdf:load-op :clim-listener)'. See the McCLIM
installation notes for other things you might want to do. [See note
#1]. If you load the clim-examples system, you cause the CLX backend
to be loaded too; after this you currently need to set a default
backend with '(setf climi:*default-server-path* :beagle)' to avoid
problems.


Note #1: Some of the examples provided with McCLIM do not execute when using
         the Beagle back end, either because of unimplemented features in
         the back end or because of lossage in the back end. Reports and
         patches would be appreciated!

Note #2: Yes, this is a little silly. For a while the Beagle back end could
         be built into its own bundle by making use of Mikel Evins'
         Bosco framework. This framework is currently undergoing some change
         in any case so I decided to make the initial CVS version available
         without providing this nicety. Hopefully in the future (yes, this
         is a TODO!) this facility will be made available again.
         Another alternative would be to incorporate the same hacks that the
         OpenMCL Cocoa examples use in order to convince Cocoa that the
         currently-executing (non-graphical) application can have an event
         loop.

Note #3: If you'd rather run with the CLX back end, load CLX
         instead here. It is possible to run multiple ports simultaneously
	 so that both a CLX and a Beagle Listener can be run side by side
	 for comparative purposes (or just because the Listener is actually
	 usable for something useful when running under CLX).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

CONFIGURATION

frame manager:
--------------
The Beagle back end defines two frame manager objects; one is the aqua
look and feel, the other is the 'standard' McCLIM look and feel. If you
want to configure a specific frame manager to be used, set the following
parameter:-

CLIM:*DEFAULT-FRAME-MANAGER*
  -> 'beagle:beagle-aqua-frame-manager  [default]
  -> 'beagle:beagle-standard-frame-manager

Note that as yet, no native (aqua) look and feel panes have been defined,
so it doesn't matter which frame manager you use.

multiple ports:
---------------
If you have an X server running and have loaded both the beagle and clx
backends, you can mix and match. For example, by issuing the following:-

(in the following, '>' is the terminal prompt, '?' is the OpenMCL
 command-line prompt, '->' is the OpenMCL Listener prompt and 'CL-USER>'
 is the CLX Listener prompt)

1.  > openmcl
2.  ? (require "COCOA")
3.  -> (require "ASDF")
4.  -> (load "home:load-clim")
5.  -> (load "home:load-clx")
6.  -> (load "home:load-beagle")
7.  -> (setf climi:*default-server-path* :clx)
8.  -> (clim-listener:run-listener)
9.  CL-USER> (setf climi:*default-server-path* :beagle)
10. CL-USER> (clim-listener:run-listener :new-process t)

I can get both a CLX and a Beagle Listener running simultaneously.
You can also do a (clim-listener:run-listener :new-process t) in (8)
and then run the other listener from the OpenMCL Listener.  Other
variations probably work too, but I haven't experimented too much.

(7) isn't actually necessary, since the CLX port appears in the server-
path search order before the Beagle port does.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

KNOWN LIMITATIONS / TODO LIST

--- Button pane highlighting and mouse event handling ---

!5.  Mouse down / up on buttons appears not to work very well unless the frame
    containing the buttons is the only active frame.
    Actually, this ^^^ seems to work fine, but the highlighting for button
    gadgets looks screwy under OS X.
    (Think there is a problem with tracking rectangles not being set for
    panes. Another alternative relates to the calculation of pointer position
    in the MOUSE-ENTER/EXIT event generator.)

    Also, think 'drop down' menus aren't working for similar reasons; either
    to do with McCLIM not understanding where the pointer is, or something to
    do with tracking-pointer.

    (Doesn't appear to be anything to do with tracking pointer...)


!21. Highlighting on mouse overs isn't quite right; artefacts are left on the
    display after the mouse has moved out of the target object bounding
    rectangle (most easily visible in CLIM-FIG again, and also in the
    directory view of the Listener (look at the highlighting of the images).


--- General windowing and drawing ---

!24. Testing + further work on patterns and stencils.


!26. Minimising frames and then restoring them leads to the frame not
    being drawn properly; it looks like 'drawRect' is invoked (as
    expected), but nothing to tell McCLIM to redraw the whole frame.
    Suspect a notification is sent... needs investigation.


--- Large output histories ---

!23. Large output histories: the transformations and geometry calculations
    go wrong when the output takes up more than 2^16 pixels; the medium
    should be used to account for this (it does in CLX) but for some
    reason it isn't. Can work around by changing every #x8000 in
    UPDATE-MIRROR-GEOMETRY (see sheets.lisp in core) to #x800000 (or larger)
    but this will fail eventually (i.e. with a large enough output
    history), so it needs sorting properly.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

NATIVE PANES

The following panes have native equivalents...


From the spec.:-

    Spec. name                 McCLIM class                Cocoa equiv  Impl?
    -------------------------------------------------------------------------
    BASIC-PANE                 BASIC-PANE
    HBOX-PANE                  HBOX-PANE
    VBOX-PANE                  VBOX-PANE
    HRACK-PANE                 HRACK-PANE
    VRACK-PANE                 VRACK-PANE
    TABLE-PANE                 TABLE-PANE                  NSTableView  [ ]
    GRID-PANE                  GRID-PANE
    SPACING-PANE               SPACING-PANE
    OUTLINED-PANE              OUTLINED-PANE               NSBox        [ ]
                               BORDER-PANE
                               RAISED-PANE
                               LOWERED-PANE
    RESTRAINING-PANE           RESTRAINING-PANE
    BBOARD-PANE                BBOARD-PANE
                               VIEWPORT-PANE
    LABEL-PANE                 LABEL-PANE                  NSBox        [ ]
    SCROLLER-PANE              SCROLLER-PANE               NSScrollView [ ]
    CLIM-STREAM-PANE           CLIM-STREAM-PANE
    INTERACTOR-PANE            INTERACTOR-PANE
    APPLICATION-PANE           APPLICATION-PANE
    COMMAND-MENU-PANE          COMMAND-MENU-PANE
    TITLE-PANE                 TITLE-PANE
    POINTER-DOCUMENTATION-PANE POINTER-DOCUMENTATION-PANE 

    PUSH-BUTTON-PANE (g)       PUSH-BUTTON-PANE            NSButton     [x]  (32)
    TOGGLE-BUTTON-PANE (g)     TOGGLE-BUTTON-PANE          NSButton     [ ]
    MENU-BUTTON-PANE (g)       MENU-BUTTON-PANE            NSMenuItem   [ ]
    SCROLL-BAR-PANE (g)        SCROLL-BAR-PANE             NSScroller   [x]
    SLIDER-PANE (g)            SLIDER-PANE                 NSSlider     [x]
    RADIO-BOX-PANE (g)         RADIO-BOX-PANE              NSMatrix     [ ]
    CHECK-BOX-PANE (g)         CHECK-BOX-PANE              NSMatrix     [ ]
    GENERIC-LIST-PANE (g)      GENERIC-LIST-PANE
    GENERIC-OPTION-PANE (g)    GENERIC-OPTION-PANE         NSComboBox   [ ]
    TEXT-FIELD-PANE (g)        TEXT-FIELD-PANE             NSTextField  [ ]
    TEXT-EDITOR-PANE (g)       TEXT-EDITOR-PANE            NSTextView   [ ]


McCLIM extensions:-

    McCLIM class
    ------------
    BOX-ADJUSTER-GADGET                                    NSSplitView  [ ]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

1.  Speed! The current implementation is __slow__, especially when there is a
    large output history. Paolo's stress test takes 26 seconds and conses
    16MB on my (admittedly slow) iMac compared to 1.5 seconds on a 2.4GHz
    Pentium IV and unknown (to me) consing.

    NB: the number of subclasses of T when running Beagle is about twice the
        number when running CLX (because the Cocoa bridge introduces every
        Cocoa object into the Lisp image, and even just the core + appkit
        frameworks are large). Additionally, I think allocated Cocoa objects
        are included in the cons measurement of the Lisp image which accounts
        for a chunk of the memory usage.

    Should be able to speed things up by not setting drawing options unless
    necessary. A hammer-like approach to do this has been implemented but could
    do with a little finessing.

    UPDATE - 25.APR.2005 - When the mirror transformation is set (sheet is
                           scrolling) we dispatch a repaint on the 'untransformed
    mirror region' (using the mirror transformation as the 'untransformation')
    instead of on the whole sheet. Things seem to behave better (i.e. quicker) now.
    This HASN'T made '(time (clim-listener::com-show-class-subclasses t))' execute
    any faster though (or cons less). We're doing way too much work drawing stuff
    I think, and because we get CLIM to redraw the regions (linear search through
    output history?) it's not too fast. Suspect in CLX when the sheet is scrolled,
    no redraw happens from CLIM generally. Also, Cocoa appears to get really slow
    at rendering text when the output history gets too large (maybe this is CLIM
    again, it's hard to know). Need to profile.

    TODO: Use approximation for text sizing (there's as much overhead working out
          how big rendered text would be as there is to rendering the text itself,
          or almost).

    TODO: Use pixmap for mirror contents and use blitting to copy areas?


2.  When running the Listener (and other applications), the resize
    handle is not visible; it's there, but you can't see it. Grab and drag
    with faith and it should work anyway.


7.  Keyboard events are not handled "properly" as far as any OS X user will
    be concerned; only the ASCII characters are recognised, along with
    simple modifiers. It's enough to enter commands and edit the command via
    Emacs-like CTRL-B (backward char), CTRL-F (forward char), CTRL-A (start
    of line), CTRL-E (end of line), CTRL-D (delete char).

    Suspect some changes to McCLIM core will be needed before this stuff
    can be supported 'properly'.


8.  There is no +flipping-ink+ implementation. Anything drawn in flipping
    ink shows up bright red with about 50% transparency (which is why the
    cursor looks so strange).

    NB: moving to 10.4 (Tiger) would fix this since there's an XOR mode
    on NSBezierPath in that OS version. So too would implementing pixmaps
    for mirrors, since we can do XOR image compositing. My gut feel is I'd
    like to avoid having pixmap caches for mirrors though.


12. There's some debug output remaining in some corner cases.


22. Sending key-down / key-up events for modifiers-changed events doesn't
    look to help get the pointer documentation pane to show the correct
    prompt. For example, in the Listener, issue a 'help commands' and
    mouse over one of the elements. The PDP says 'R: Menu / To see other
    commands, press Super'. If super is pressed, the highlighting on the
    presentation is lost, and the PDP is blanked out. Only when the mouse
    moves again is the PDP redisplayed correctly, with 's-L: Describe
    Presentation'.
    Need to check CLX implementation to see if this is the same...


27. Since key focus handling was implemented, closing apps often lands
    you in the kernel debugger. As an example, running glimpse, then
    the Listener, type something in Listener, give glimpse focus, exit
    glimpse, type on Listener -> *boom*. Suspect some reference to
    glimpse objects are being kept around when they shouldn't be.


28. If a command is entered in the Listener which generates a presentation
    immediately underneath ('show class subclasses' is good, the top
    node is immediately under the command), and this presentation is
    highlighted, when it is unhighlighted the command is cleared also.
    It's still present in the output history, and mousing over it makes
    it reappear, but still... not sure why this happens.


29. Event signal-semaphore and consume-semaphore code isn't quite right;
    if the user generates events whilst Beagle is in the middle of a
    long-lived operation (generating a big graph, for example), some of
    those events are 'trapped' in the queue until other events take place.
    Looking at the code, I don't think this should happen... (but it does).

    NB. (11.JUN.2005) I haven't observed this since the move to 0.14.3
    Hopefully it has been resolved within OpenMCL.


30. Event handling over 'drop down' menus is strange; after clicking on the
    menu name, all events appear to be blocked until the mouse button is
    released (no drag events or anything). After release, the events are
    processed (but then it's too late, the menu is gone). Note that this is
    *nothing* to do with tracking pointer, which appears not to be used in
    drop down menus (only popup menus, which work, more or less).

    (This is also a problem for context menus, if you keep the right button
    depressed (menu isn't drawn)) [yes, it is - no problem with popups that
    I can see now, other than whilst the button is held down the events
    aren't routed to them].


31. Using the scroll wheel over (aqua) scroll panes doesn't work (but does
    when used over the appropriate viewport).

    Also, I'd like to hide the scrollbar when there's no scrolling to be
    done (lozenge size = 1.0), but I can't even deactivate them at this
    point!

    BEAGLE-SCROLL-BAR-PANE should inherit from SCROLL-BAR rather than
    SCROLL-BAR-PANE I think.


32. Using a button to fire up a new frame is problematical; the callback
    for the button doesn't return, so the button never releases focus and
    no events are processed. Specific to aqua buttons.


33. When a window is closed by the (x) button it should be cleared up
    properly; specifically, its event loop etc. should be gced. Appears
    at the moment that this isn't happening.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

FIXED STUFF PREVIOUSLY ON THE KNOWN LIMITATIONS / TODO LIST


-3.-  There are not yet any aqua look and feel panes. Sorry, I'm trying to
    get everything else working first!

    UPDATE 11.JUN.2005 - I have implemented (90+%) native scroll bars, and
                         am looking at implementing other native panes over
                         time. These will be added over time.


-4.-  Pixmap support is not implemented; this means clim-fig drawing doesn't
    work.
    This is getting there, although not very efficiently; we are missing a
    method for (sheet-mirror mirrored-pixmap). This is evident if you run
    (clim-demo::clim-fig) and actually do some drawing.
    RESOLVED 08.AUG.04 [NB. this functionality is not too efficient I think
                       and needs revisiting (like everything else does)]

-4.5.- Designs (other than colours) aren't implemented - THIS means there are
     no icons in the Listener.
    UPDATE 25.APR.2005 - This is done now, more or less (done for tiled patterns,
                         hacked for non-tiled patterns, not looked at for
                         stencils).

-6.-  Swapping between key windows (the window accepting the keyboard input)
    is a little flakey; as an example, if a second Listener is started from
    the first, clicking between the windows transfers key focus (as
    expected). However, if the first is then 'Exit'ed, the second will not
    get the key focus until some other (non-McCLIM) window has been given
    the keyboard focus first (i.e. click on the OpenMCL Listener window,
    then back on the McCLIM Listener window).
    Additionally, clicking on a scroll-bar (for example) makes the window
    key, so clicking on a view that accepts keyboard input (interactor)
    within this window won't then allow keyboard input.
    We should stop scroll-bars being able to get keyboard input...
    FIXED 17.MAY.2005 DAR - the problem was we never invoked 
                            'setf (port-keyboard-input-focus' in handling
    the 'did become key' notification.

-9.-  Foreign memory management is non-existent. This is fine whilst running
    in the same thread as the OpenMCL Listener (since we use its autorelease
    pool) but when running in a separate thread lots of warning messages
    are generated. You might find it necessary to force-quit OpenMCL after
    you've finished.

    UPDATE: have made one pass ensuring heap allocated memory (with
    MAKE-RECORD) is free'd, and that retained objects are released. Things
    seem much better now but I'm sure I overlooked a few.
    Moving this to 'resolved' (but keep eyes open for more occurrences ;-)

    Note: this also seems to have given a bit of a speed-boost; perhaps we
    avoid swapping, or have cut down on GC overhead.

-10.- Text sizes aren't calculated correctly; when multiple lines are output
    together, the bottom of one line can be overwritten by the top of the
    next line.
    UPDATE 21.AUG.2004 - I think the text sizes *are* calculated correctly.
                         For some reason commands entered in the listener
    appear to be thought to be a couple of pixels shorter than output
    presented in the listener which is weird. If you issue 'help commands'
    in the listener, then mouse over the middle-top command and move the
    mouse off that that entry, the 'Help (with) Commands' input is cleared.
    Perhaps Cocoa thinks the dirty region includes that text or something.
    It's annoying whatever. Still, I'm going to mark this as fixed for now
    and maybe will come back to it later.

    TODO: I think this is to do with the way width and height (rather than
          text-size) is used to calculate bounding rectangles might be
          wrong (i.e. getting the wrong information from Beagle).

    TODO: Also note that when a graph is output, there's no (significant)
          problem with bounding rects etc. I suspect Beagle may be drawing
          with y-align :bottom when CLIM is expecting :baseline, or
          something.

-11.- Line dash patterns haven't been implemented.

-13.- Some Apropos cases fail; for example 'Apropos graft' fails (although
    '(apropos 'graft)' does not). The same problem prevents the address
    book demo working too I think. [This appears to be caused by treating
    any 'NIL' which should be output as a literal object. Not sure how
    this is happening, but it should be possible to track it down].
    RESOLVED 21.AUG.2004 - it appears MACPTRs are output using the family
    (for a text style) of :fixed - which didn't exist (only :fix). Not
    sure if this is a specification violation or not... both are bound
    in McCLIM, so suspect :fixed is added for compatability with one of
    the vendor CLIMs?

-14.- Not all foreign objects we keep hold of in the back end are heap-
    allocated. Some are stack-allocated and cause errors about 'bogus'
    objects once they go out of scope. At least, I think (and hope) that's
    the reason 'cause that's easy to fix. RESOLVED 17.JUL.04

-15.- Popup menus don't work quite the same way as they do in the CLX back
    end. Cocoa doesn't support pointer grabbing so disposing of menus when
    the mouse pointer moves off them doesn't work in Beagle (either choose
    a command, or mouse-click on a different window to get rid of them).
    Additionally, highlighting the menu item the mouse is currently over
    is rather intermittent, although the correct menu item appears to always
    be chosen on mouse-click (related to the same tracking rectangle issue
    mentioned in (5)?).

    Further observation; if the right button is held down whilst moving
    around the popup, the bounding rects are drawn properly. It's only
    intermittent if the right mouse button is released. Not sure if this
    makes sense... (note: right-click to bring menu up, the right click
    again and 'drag' to get menu highlighting working). Not sure if this
    is because the menu frames don't process mouse moved events properly.
    Could be.


-16.- Windows are put on screen very early in the realization process which
    wasn't a bad thing during early development (could see how far through
    things got before blowing up) but now it just looks messy.

    This is now resolved for application frames (put up when ENABLE-FRAME
    is invoked) but not for menu panes (since ENABLE-FRAME appears not
    to be invoked at all for them). Resolved (kind-of) for menu frames too
    now; these are put up during the ADOPT-FRAME functionality.

    Drawing is still rather messy though, more investigation needed.


-17.- *BEAGLE-DEFAULT-FRAME-MANAGER* should be replaced with the standard
    *DEFAULT-FRAME-MANAGER* instead.
    FIXED 17.MAY.2005 - *beagle-default-frame-manager* is no more (well...
    it's still there but it can be ignored to all intents and purposes).

-18.- Note about force-quit; appended to (5).

-19.- Menus don't work in CLIM-FIG (or anywhere else!). No idea why not...
    This is because (I think) the menu popups don't operate in a flipped
    coord system (unlike NSViews). [Command menu that is drawn across
    top of window has 'child menus' drawn in bottom-left corner of screen]

    TODO: make use of graft native transformation to flip coords rather
          than the NSView 'isFlipped' method?


-20.- Bounding rectangles are slightly off (this can be seen in CLIM-FIG again).
    It's only a matter of a pixel, maybe 2 in the worst case I've seen.
    Probably caused by rounding errors in Beagle (we do quite a lot of
    int -> float conversion and ordinate manipulation (in cocoa 0.0, 0.0 falls
    'between pixels' - 0.5, 0.5 is 'center of pixel').

-25.- Bounding rects for commands entered in interactor panes are way out;
    looks like the baseline of the text is being used as the bottom of
    the bounding rect!


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

WISH LIST

1.  Bring Beagle back end into line with CLX back end in terms of supported
    McCLIM functionality (basically - -pixmap-support-, flipping ink and -line
    dashes-)

2.  Implement native look and feel

3.  Integrate the OpenMCL break-loop into the McCLIM Listener; at the moment
    you end up dumped either in the OpenMCL Listener or the terminal window,
    depending (on what I haven't quite worked out) - generally the former.

4.  Look again at the build process and reintegrate with Bosco.

-5.-  Possibly migrate to being a Carbon, rather then a Cocoa, application to
    remove OpenMCL version dependencies. *Don't bother doing this. Note that
    in 10.4, Cocoa apps take advantage of GPU caching (performed by the OS),
    but Carbon apps do not. It's possible in some future OS version that
    Cocoa drawing will actually be faster than Carbon drawing.*

-6.-  Reduce focus locking in NSViews (I think this will give a not
    insignificant speed increase).

7.  Documentation

8.  Code tidying, and lots of it! Refactoring. Need to implement many
    abstractions. (which should also help in the Cocoa -> Carbon move,
    when it happens).

9.  Release resources on exit.

10. Test / fix multiple port, multiple screen, multiple frame managers.

11. Write general test suite for McCLIM in general and Beagle back end in
    particular.

12. Look again at sheet hierarchy stuff; I'm pretty sure this only works
    when the graft is in the default orientation. (There's lots of
    problems in this area, most of which are in Beagle, some of which
    are in McCLIM (by design? But not sure which are by design), e.g.
    sheets being mirrored but with no medium attached).

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
