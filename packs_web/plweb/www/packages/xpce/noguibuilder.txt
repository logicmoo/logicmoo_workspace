---+ Why GUI-Builders are evil

GUI-Builders are there to make the life of a programmer easier by
reducing the learning curve, and let you create better applications
faster. This page questions these claims.

GUI-Builders provide a GUI to specify a GUI. Below we summarise the
technical requirements for designing and implementing a GUI.

    $ Appearance :
    A GUI consists of windows displaying various controls that allow the
    user to examine data of the application and interact with the
    application. The community has established a large number of
    standard controls such as buttons, various styles of menus etc. When
    designing GUI windows, controls appropriate to the users task have
    to be selected, configured and placed on the window.

    A GUI-Builder provides lists of available controls, menus for
    configuring these controls and direct-manipulation editors for
    placing them on the window.

    $ Non-standard controls :
    Using standard controls is not always a good solution. For some
    applications, it is feasible to design graphical representations
    that render the information naturally. Using such representations
    and controlling the application through direct-manipulation of the
    representation results in a better interface.

    $ Specification of interface dynamics :
    Most applications do not consist of a single window with statically
    placed controls. Windows for different sub-tasks are presented to
    the user. The content of a window is dynamically built, based on
    the current context of the application and possibly user-modelling
    or dynamic planning of (sub-)tasks.

    $ Relating the interface to the application :
    The GUI controls need to call application code and the application
    needs to control the GUI to represent changes of its state. Various
    mechanisms, both using a GUI-Builder and otherwise exist to deal
    with this. Roughly speaking there are two alternatives. One relates
    controls to call-back procedures in the application and the other
    enforces the model-view model for separating application and GUI.

    $ Abstraction :
    In GUI-Applications we can often identify reuse of controls,
    combinations of controls and behaviour. This should be encouraged
    because it makes the application easier to understand for the user.
    Ideally, this reuse should be reflected in the code to improve
    maintainability.

---++ So, what does the GUI-Builder do for us?

GUI-Builders provide a natural and appropriate mechanism to select and
position a static configuration of controls in a window. If the window
serves as a view for some model and the construction of this model fits
the assumptions made in the GUI-Builder, it provides adequate mechanisms
to link the application to the GUI.

Abstraction, rules for defining dynamic building as well as dynamic
relations between components are tackled poorly by graphical interfaces,
while these aspects have great influence on notably the maintainability
of interface code.

---++ Alternatives

The power of Prolog lies in its declarative nature and the ease to
implement rules working on this representation. A well designed Prolog
application is founded on a simple, easy-to-maintain Prolog fact-base. A
well designed GUI implementation should exploit this foundation as much
as possible.

Suppose we have several entities in our application with some
properties. The GUI should be able to create, view and update instances
of this entity. Using `modern' Object Oriented technology and a GUI
builder, we would define classes to reflect each of the entities and use
the GUI-Builder to design dialog windows for each of the entities. If we
have a good GUI tool, we can associate these using a model-view relation
and the communication will be set-up automatically.

This approach is not ideal. Suppose we will also add context help to the
system. We end up with many locations where strongly related information
is stored: the class-definitions stores the attribute-names and their
types, the method-implementation encode constraints and relations in the
data-model, the GUI-Builder defines the control-selection and layout and
the help-file the associated text.

When using XPCE/Prolog we use a *|Prolog fact-base|* that describes all
relevant information about the entities in a single location. We will
use this information to maintain the database (either in Prolog, as XPCE
objects or in an external database), evaluate constraints on the data,
generate the dialog windows and provide (multilingual) context sensitive
help.

This is feasible due to the *|meta-programming|* capabilities of Prolog
and the strong support for *|symbolic layout|* management of XPCE.

The learning curve for designing applications this way is certainly
longer. Once on steam however, you will see your productivity rise
sky-high while your programs are clear and easy to maintain!
