(*!  DATA OBJECT MODULE *)
INTERFACE RuleEngine;

(** This is the main interface of subsystem RuleEngine. *)
(***************************************************************************)
(** Created by:  Roland Baumann						   *)

(** $Author$
    $Revision$
    $Date$
    $Log$
    Revision 1.1  2003/03/27 15:25:40  hosking
    Initial revision

    Revision 1.6  1998/08/12 11:04:45  roland
    Efficiency improvement: RuleEngine notifies EventDetectors of
    registering and unregistering triggers. In this way, events need only
    be signaled when triggers are registered.

    Revision 1.5  1998/01/21 14:13:31  roland
    Comments corrected.

    Revision 1.4  1997/11/12 17:23:12  roland
    WaitForRemoteActions now does a Thread.AlertWait, so that the waiting
    thread can be interrupted.

    Revision 1.3  1997/11/12 15:33:27  roland
    Bugfixes in support for specialized EventHandlers.

    Revision 1.2  1997/11/03 12:40:26  roland
    New procedures to check connection to rule server.

    Revision 1.1  1997/10/31 14:02:59  roland
    The RuleEngine subsystem implements an event-trigger mechanism for GRAS.
    It is splitted into local and remote rule handling.

*)
(***************************************************************************)

(* The RuleEngine realizes active behavior in GRAS.  A rule or trigger
   consists primarily of an event specification, called event pattern, and
   an action.  Its semantic is, that the action is executed when an event
   matching the triggers pattern is signaled.  We say the trigger is fired
   (or the rule is triggered).  Simple as this sounds, there are quite a
   few details on how to control rule triggering and action execution,
   though.  We explain them below. *)

IMPORT CardSeq, Thread;
IMPORT Trigger, EventDetector, EventHandler, Event;

(* ------------ Initialization and set up --------------- *)

PROCEDURE Login (    host, id        : TEXT;
                 VAR remoteMonitoring: BOOLEAN;
                 VAR error           : TEXT     );
  (* Initializes the rule engine.  The rule engine tries to connect to the
     rule server via name server on machine 'host' registered with 'id'.
     The return value remoteMonitoring indicates whether this connection
     was successful.  If not, 'error' states a reason in readable form.
     Even if remoteMonitoring is FALSE local event handling will work
     without problem. *)

PROCEDURE Logout ();
  (* Disconnect from the rule server *)

PROCEDURE CheckRuleServer (VAR connected: BOOLEAN; VAR msg: TEXT);
  (* Check the connection to the rule server.  If no connection to the rule
     server exists, connected will be set to false and msg describes the
     error situation. *)

PROCEDURE RegisterTransactionUnit (): CARDINAL;
  (* The RuleEngine supports a transaction semantic in that it offers three
     distinct coupling modes for a trigger.  Couplings modes determine when
     an action is executed in relation to the transaction that triggered
     the event.  A transaction unit is a program unit with transaction
     semantics.  In GRAS a graph pool is a transaction unit.  Each pool
     registers itself with this procedure to the rule engine.  The
     transaction unit identifier returned by RegisterTransactionUnit must
     be given as parameter to context, signal and transaction procedures of
     this interface. *)

PROCEDURE UnregisterTransactionUnit (unit: CARDINAL);

PROCEDURE RegisterEventDetector(detector: EventDetector.T; types: CardSeq.T);
  (* Register detector as a source for events of the types in types.
     A detector will be notified if a trigger for any of the events in
     types is registered or unregistered. In this way, the detector only
     needs to signal events when triggers are present. *)
  
PROCEDURE RegisterEventHandler (handler: EventHandler.T; types: CardSeq.T);
  (* Register handler as event handler for the event types in types.  All
     occurring events of these types will be passed to handler after
     calling this procedure.  Note: It is not checked whether another
     handler for some of the types already is registered. *)

(* ---------------- Remote signaled events -------------- *)

TYPE
  Notifier = PROCEDURE ();
  NotificationTime = {Always, OutsideTransaction};

PROCEDURE WaitForRemoteActions (lock: MUTEX) RAISES {Thread.Alerted};
  (* Locks lock and waits until the remote rule handler signals incoming
     actions. *)

PROCEDURE RegisterRemoteActionNotifier (n: Notifier; when: NotificationTime);
  (* When the rule server signals an event, the client might not be
     executing GRAS operations, so that the triggered actions are never
     executed (because the execution cycle is not started after a commit to
     the top-level transaction).  If a notifier is registered, the
     RuleEngine calls this procedure if remote events are signaled.  n is
     called either every time a remote event arrives
     (NotificationTime.Always) or only if no transaction is active
     (NotificationTime.OutsideTransaction).  Note: notification is done in
     a separate thread.  If you want the main thread to execute the
     triggered actions, you must propagate the notification to the main
     thread via signals or some other mechanism (writing on a file
     descriptor is a very popular method for notifying window managers). *)

PROCEDURE ExecuteActions (unit: CARDINAL);
  (* Inside a transaction, if action execution is not delayed, the
     immediate action execution cycle (see below) is started.  This should
     have no effect at all.  Outside transactions, the decoupled cycle is
     started, which will also execute remote actions.  This is especially
     useful, when the program was notified about remote actions and normal
     operation wouldn't eventually start action execution automatically. *)

PROCEDURE ExecuteRemoteActions ();
  (* If action execution is not delayed, a special action execution cycle
     is started.  The cycle handles remote actions right after immediate
     actions.  This is especially useful, when the program was notified
     about remote actions and these should be executed immediately. *)

(* ---------------------- Triggers ---------------------- *)

TYPE Interest = {Self, Others, All};

PROCEDURE RegisterTrigger (trigger : Trigger.T;
                           interest: Interest;
                           userdata: REFANY      := NIL): CARDINAL;
  (* Registers trigger with the corresponding event handler.  The returned
     number serves as unique identifier for this trigger.  If interest is
     Self, only local events are monitored.  Interest.Others specifies to
     monitor only remote events and Interst.All monitors both.  The
     optional userdata will be returned when an event is signaled. *)

PROCEDURE UnregisterTrigger (id: CARDINAL);
  (* Unregisters trigger with id *)

(* ---------------------- Contexts ---------------------- *)

PROCEDURE ActivateContext (tu: CARDINAL; context: CARDINAL);
  (* Activates context as declared with ContextSet.  Only triggers will be
     fired which inhibiting contexts are all inactive and permitting
     contexts are all active.  Use ContextSet interface to declare new
     contexts and handle context sets. *)

PROCEDURE DeactivateContext (tu: CARDINAL; context: CARDINAL);
  (* Deactivates context as declared with ContextSet. *)

(* --------------------- Event signaling ---------------- *)

PROCEDURE Signal (tu: CARDINAL; event: Event.T);
  (* Signals event.  All matching triggers are executed (or collected for
     later execution, depending on coupling mode etc.  Cf.  below. *)

PROCEDURE DelayActionExecution (tu: CARDINAL);
  (* Delay execution of triggered rules until commit of the current
     (nested) transaction.  Note: Following DelayActionExecution calls have
     no effect until the current transaction commits ore aborts or a call
     to ReleaseActionExecution succeeds. *)

PROCEDURE ReleaseActionExecution (tu: CARDINAL);
  (* If action execution is delayed because of a call to
     DelayActionExecution within the same transaction (level), action
     execution resumes. *)

(* --------------------- Transactions ------------------- *)

(* Note: The following procedures are called by GRAS automatically.  You
   should not use them unless you know very well what you are doing. *)

PROCEDURE NotifyBeginTransaction (tu: CARDINAL);
  (* Increments transaction level *)

PROCEDURE PreCommitTransaction (tu: CARDINAL);
  (* Executes all deferred actions, except when action execution is
     delayed. *)

PROCEDURE PostCommitTransaction (tu: CARDINAL);
  (* Decrements transaction level.  If level is 0 after decrement, all
     decoupled action are executed. *)

PROCEDURE NotifyAbortTransaction (tu: CARDINAL);
  (* Cancels all actions which were triggered by this or any nested
     transaction *)

(* 1) Events, event types, and event patterns

   Events (RuleTypes/Event.i3) have a type and several attributes.  The
   type of an event must be defined with the EventTypes subsystem
   (RuleTypes/EventTypes/EventTypes.i3 and
   RuleTypes/EventTypes/EventType.i3).  GRAS declares several types, e.g.
   NodeCreation/Deletion etc.  To find out what types are defined and which
   attributes events of these types have, look at the GRAS interfaces
   defining them (alternatively you can querie the EventTypes subsystem to
   find out).

   An event pattern (RuleTypes/EventPattern.i3) like an event has a type
   and attributes.  Attributes of an event pattern can have the same values
   as attributes of an event of the same type but may also be wildcards.
   Event patterns are used to define triggers.  A trigger fires, if a
   signaled event matches its event pattern.  The matching between events
   and event patterns is defined as follows.

   Def: An event e matches an event pattern p, iff the types of e and p are
   the same and for each attribute a of p/e holds: p.a = e.a or p.a =
   wildcard.

   2) Triggers, contexts, coupling modes

   Though its main constituents are event patterns and actions, a trigger
   has some more attributes, that control its firing and execution.  First
   of all, there is the notion of context.  A context is symply a name
   defined by a part of the application (e.g.  "Undo" defined by GRAS'
   change management layer).  This name is associated with a number when
   the context is declared (cf.  RuleTypes/ContextSet.i3).  Contexts can be
   in one of two states, activated or inactivated.  The rule engine keeps a
   set of activated contexts.  This helps avoiding unnecessesary firing of
   rules, because each trigger has an inhibiting and a permitting set of
   contexts.  It can only fire, if all its inhibiting contexts are inactive
   and all of its permitting contexts are active (so a trigger with empty
   inhibiting and permitting context sets can always fire).  If you were
   interested, e.g.  in events signalling the createion of a node, but not
   if this creation is done during an undo, you would put the "Undo"
   context in the corresponding triggers inhibiting context set.  The
   change management layer will always activate the "Undo"-context if it
   performs an undo, and hence the trigger would not be fired during undo.
   Note that there is an upper bound to the number of declared contexts
   (currently Word.Size).

   Each trigger also has a coupling mode which specifies when the action
   should be executed in relation to the transaction in which the event was
   signaled.  If the coupling is immediate, the action will be executed
   immediately after the event was signaled (i.e.  before the call to
   SignalEvent returns).  When the coupling is deferred, the triggers
   action will be stored until the end of the current transaction.  GRAS
   signals PreCommitTransaction before each commit, and this leads to
   executing all collected deferred actions.  With coupling mode decoupled,
   finally, the action is executed after commit of the top-level
   transaction.  Note that the RuleEngine does not start GRAS-Transactions
   on its own, instead a decoupled action has to do this before it accesses
   a GRAS graph.

   If an event is signaled which triggers more than one rule, the order of
   action execution is determined by the rules priority.  Rules with higher
   priority are handled first.  Note that the execution ordering due to
   coupling modes is stronger than priority ordering.

   3) Delaying action execution

   The execution of actions can be delayed by the application program.
   This is useful if it is necessary to atomically execute a number of
   database updates without interference from executed actions.  The call
   to DelayActionExecution is associated with the current transaction
   level.  As long as the transaction level is equal to or higher to the
   level during the call, actions will not be executed.  At the end of the
   transaction in which DelayActionExecution was called, all collected
   action are executed and action execution is released for the
   parent/sibling transactions.

   A call to ReleaseAtionExecution has only an effect on the rule engine,
   if action execution is delayed due to a call to DelayActionExecution at
   the same transaction level.  The call to ReleaseAtionExecution executes
   all collected actions of triggers with immediate coupling and returns.
   Collected action of triggers with deferred coupling even if triggered in
   child transactions, are executed only at the end of the current
   transaction.

   When DelayActionExecution is called when action execution is already
   delayed, (be it due to a call to DelayActionExecution in the same
   transaction or an ancestor transaction) this call is ignored.  To make
   sure, your code is executed without interference, first call
   DelayActionExecution then start a new transaction and call
   ReleaseActionExecution after the corresponding transaction commit.

   4) Remote event monitoring and rule invocation

   The rule engine does not only detect events signaled from its own GRAS
   client (local events), but can also cooperate with the rule engines of
   other GRAS clients.  If you are interested in the event occurrences of
   other clients, you can specify Interest.Others or Interest.All when
   registering a trigger, to monitor only events of other clients (remote
   events) or local and remote events, respectively.  If you specify
   Interst.Self, then only local events trigger the rule.

   To be able to monitor remote events, the rule engine must connect to a
   rule server during login.  Even if it is unable to connect to the
   server, it will still continue to monitor local events.  Actions of
   remote events are normally only executed in decoupled mode after
   processing of all local events.  The reason for this is, that these
   events wil in general not contribute to local computations.  Hence their
   interference might have unexpected or unwanted results.

   To override this behavior, one can register a notification procedure
   with the rule engine, which will be called every time a remote event was
   reported from the rule server.  To do so, call
   RegisterRemoteActionNotifier with NotificationTime.Always.  Being
   notified, an application might explicitly call ExecuteRemoteActions to
   start a special execution cycle (see below) which executes remote
   actions even when inside a transaction.

   Registering a remote action notifier can be useful even if remote
   triggered action should only be executed by the normal execution cycle.
   When your application program is idle with repsect to GRAS, i.e.  it
   does not run a GRAS transaction at the moment, the rule engine will not
   start an execution cycle, even if remote events arrive.  It will,
   however, call a registered notifier.  If the NotificationTime-Parameter
   was NotificationTime.OutsideTransaction, the rule engine only calls back
   when the transaction level is 0, i.e.  when no GRAS transaction is
   running.  A call to ExecuteActions will then start the normal execution
   cycle, leading finally to the execution of the remote triggered actions.

   A crucial point for this notification procedures is that they are called
   in a separate Modula-3 thread.  If your application isn't multi-thread
   safe (which it is not if it uses GRAS), it might be a good idea to
   execute the actions in the main thread.  The registerd notifier has to
   deal with this.  An mechanism for notifying the main thread when this
   executes a window system event-cycle, is to use a file descriptor which
   can be monitored by the window system.  When the notify-procedure writes
   on this file descriptor, the window system registers this and calls a
   procedure that was registerd by application.

   5) Action execution cycles

   When an event is signaled, an immediate trigger for this event is fired.
   and action execution is not delayed, the rule engine starts the
   immediate action execution cycle (see below).  This is also executed,
   when ReleaseActionExecution is called within the delaying transaction.
   Actions activated by events during action execution will not be executed
   immediately but will be collected and executed later within the same
   execution cycle.

   The deferred action execution cycle is started, before the commit of a
   transaction, so that all deferred actions will be executed.  This cycle
   also executes the immediate actions triggered during execution of
   deferred actions.

   Decoupled actions are executed after the commit of the top-level
   transaction in the decoupled action execution cycle.  If no more
   decoupled actions are activated, this cycle checks for remote triggered
   actions and executes them if necessary.  The cycle also calls the
   deferred action execution cycle to process actions triggered during
   action execution.

   Finally, the remote action execution cycle executes actions triggered by
   other clients.  It also calls the immediate action execution cycle to
   cope with immediate actions triggered during action execution.  This
   action execution cycle is involked only by explicit user request (a call
   to ExecuteRemoteActions inside a transaction). *)

(**
   Execution cycles

   1) Immediate action execution cycle:

      while immediate actions are triggered do
          fetch a triggered action and execute it
      end;

   2) Deferred action execution cycle:

      loop
          call immediate action execution cycle;
          if deferred actions are triggered then
             fetch one and execute it;
          else
             exit;
          end;
      end;

   3) Decoupled action execution cycle:

      loop
          if decoupled actions are triggered then
            fetch one and execute it;
          elsif remote triggered actions are present then
            fetch one and execute it;
          else
            exit;
          end;
          call deferred action execute cycle;
      end;

   4) Remote action execution cycle:

      loop
          call immediate execution cycle;
          if remote triggered actions are present then
              fetch one and execute it;
          else
              exit
          end;
      end;

*)

END RuleEngine.
