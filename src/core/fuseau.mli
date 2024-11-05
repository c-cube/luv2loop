(** Fuseau core.

    The core library contains the definition of the
    main scheduler, fibers, switches, and other foundations
    for cooperative structured concurrency.
*)

(** {2 Foundations} *)

(** The unique name of a fiber *)
module Fiber_handle : sig
  type t = private int
  (** Unique, opaque identifier for a fiber. *)

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int

  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
end

(** Atomic events.

    An atomic event is something that can occur, or not
    occur, without in-between. *)
module Event : sig
  type 'a t = {
    poll: unit -> 'a Exn_bt.result option;
    wait: (unit -> unit) -> Cancel_handle.t;
  }
  (** An event yielding ['a] *)

  (** Used for [select] *)
  type 'ret branch = When : 'a t * ('a -> 'ret) -> 'ret branch

  val select : 'ret branch list -> 'ret
  (** Select between multiple events. The first one to
      be ready wins, and the corresponding function is called.

      If no event is ready, the fiber suspends, and the [wait]
      function of each event is used to wake it up again. *)
end

type 'ret branch = 'ret Event.branch =
  | When : 'a Event.t * ('a -> 'ret) -> 'ret branch

val select : 'ret branch list -> 'ret
(** See {!Event.select}. *)

(** {2 Synchronization} *)

(** Basic channels *)
module Chan : sig
  type 'a t
  (** Basic channel for values of type 'a *)

  val is_empty : _ t -> bool

  val size : _ t -> int
  (** Current number of items in the channel *)

  val create : ?max_size:int -> unit -> 'a t
  (** New channel.
      @param max_size if specified, writers will block when trying
      to push into a channel *)

  exception Closed

  val close : _ t -> unit
  (** Close the channel. Calls to {!receive_exn} will still succeed
      as long as items remain in the channel, until the channel
      is entirely drained; then they fail with {!Closed}. *)

  val receive : 'a t -> 'a option
  (** [receive c] receives an item from [c]. Suspends if
      the channel is empty but not closed.
      Returns [None] if the channel is empty and closed. *)

  val receive_exn : 'a t -> 'a
  (** [receive_exn c] receives an item from the channel.
      Suspends if the channel is empty and non-closed.
      @raise Closed if the channel is empty and closed *)

  val send : 'a t -> 'a -> unit
  (** [send c x] sends [x] over the channel [c].
      This might suspend the current fiber if the channel is full.
      @raise Closed if [c] is closed. *)

  val ev_send : 'a t -> 'a -> unit Event.t
  (** [ev_send c x] is an event that sends [x] into [c]
      when there is room for it *)

  val ev_receive : 'a t -> 'a Event.t
  (** [ev_receive c] is an event that resolves by receiving
      a value [x] from [c] when [c] is non empty *)
end

(** {2 Utils} *)

(** Exception with backtrace *)
module Exn_bt : sig
  type t = exn * Printexc.raw_backtrace

  val make : exn -> Printexc.raw_backtrace -> t
  val get : exn -> t
  val get_callstack : int -> exn -> t
  val raise : t -> 'a
  val pp : Format.formatter -> t -> unit
  val show : t -> string

  type nonrec 'a result = ('a, t) result

  val unwrap : 'a result -> 'a
  (** [unwrap (Ok x)] is [x], [unwrap (Error ebt)] re-raises [ebt]. *)
end

(** Time measurement *)
module Time : sig
  val monotonic_time_ns : unit -> int64
  (** Monotonic time in nanoseconds *)

  val monotonic_time_s : unit -> float
  (** Monotonic time in seconds *)
end

(** Fibers.

    A fiber is a lightweight cooperative thread.
    It runs on the scheduler and can yield to other fibers,
    notably on IO polling points.
*)
module Fiber : sig
  type 'a t = 'a Types.fiber
  (** A fiber returning a value of type ['a]. *)

  (** A fiber with erased return type. *)
  type any = Types.any_fiber = Any_fiber : _ t -> any [@@unboxed]

  type 'a callback = 'a Exn_bt.result -> unit
  (** Callbacks that are called when a fiber is done. *)

  type cancel_callback = Exn_bt.t -> unit

  val return : 'a -> 'a t
  val fail : Exn_bt.t -> _ t
  val peek : 'a t -> 'a Exn_bt.result option
  val is_cancelled : _ t -> bool
  val is_done : _ t -> bool

  exception Cancelled of Exn_bt.t
  (** Exception for fibers that are cancelled. Polling points such
    as {!yield} and {!await} will raise this if the fiber has been cancelled. *)

  val on_res : 'a t -> 'a callback -> unit
  (** Wait for fiber to be done and call the callback
    with the result. If the fiber is done already then the
    callback is invoked immediately with its result. *)

  val get_current : unit -> any
  (** [get_current ()] returns the currently running fiber.
      @raise Failure if called from outside a fiber *)

  (**/**)

  module Private_ : sig
    val create : ?name:string -> unit -> 'a t
    val cancel : _ t -> Exn_bt.t -> unit
  end

  (**/**)
end

module FLS = Picos.Fiber.FLS

(** {2 IO event loop} *)

exception Inactive
(** Exception raised when trying to perform operations
    on the scheduler after it's been disposed of *)

(** Scheduler that runs fibers.

    The scheduler is responsible for running fibers that are
    ready, but it doesn't directly deal with timeouts, readiness events,
    etc. For these see {!Event_loop}. *)
module Scheduler : sig
  type t
  (** A scheduler *)

  val active : t -> bool
  (** Is the scheduler not finished yet? *)

  val dispose : t -> unit
  (** Delete the scheduler. Idempotent and thread-safe.
    This cancels the remaining fibers. The scheduler will
    stop running when they all terminate. *)

  val n_tasks_since_beginning : t -> int
  (** Number of tasks run so far in this scheduler. *)

  val get_for_current_thread : unit -> t
  (** Get scheduler from the current thread. This must be called
      from within a Fuseau fiber.
      @raise Failure if not run from within a fiber *)
end

module Event_loop = Event_loop

(** {2 Resource management *)

(** Resource pool.

    This pool can be used for buffers. It can be used for other resources
    but do note that it assumes resources are still reasonably
    cheap to produce and discard, and will never block waiting for
    a resource — it's not a good pool for DB connections. *)
module Resource_pool : sig
  type 'a t
  (** Pool of values of type ['a] *)

  val create :
    ?clear:('a -> unit) -> mk_item:(unit -> 'a) -> ?max_size:int -> unit -> 'a t
  (** Create a new pool.
    @param mk_item produce a new item in case the pool is empty
    @param max_size maximum number of item in the pool before we start
      dropping resources on the floor. This controls resource consumption.
    @param clear a function called on items before recycling them.
 *)

  val acquire : 'a t -> 'a
  (** Acquire (or create) a new resource from the pool. *)

  val recycle : 'a t -> 'a -> unit
  (** Recycle a value into the pool. The value must not be used afterwards. *)

  val with_resource : 'a t -> ('a -> 'b) -> 'b
  (** [with_resource pool f] runs [f x] with [x] a resource;
    when [f] fails or returns, [x] is returned to the pool for
    future reuse. *)
end

(** A pool of buffers to reuse. *)
module Buf_pool : sig
  type t
  (** Buffer pool.

    This type is thread-safe. *)

  val create : ?buf_size:int -> ?max_size:int -> unit -> t
  (** Create a pool *)

  val acquire : t -> bytes
  (** Take a buffer from the pool. Once done with it, the buffer
    should be {!recycle}'d. *)

  val recycle : t -> bytes -> unit
  (** Give a buffer back to the pool. *)

  val with_buf : t -> (bytes -> 'a) -> 'a
end

module Cancel_handle = Cancel_handle

(** {2 IO streams} *)

(** IO streams.

    These streams of bytes can be used for read and write operations.
    Their usage is not required, in some event loops
    such as {!Fuseau_lwt}. This is compatible with
    the {{:https://github.com/c-cube/ocaml-iostream} iostream library}. *)
module Iostream : sig
  (** Buffered output stream. *)
  module Out : sig
    (** An output stream, ie. a place into which we can write bytes.

    This can be a [Buffer.t], an [out_channel], a [Unix.file_descr], etc.
    It must be buffered to amortize the cost of individual small writes (including
    [output_char]). *)
    class type t = object
      method output_char : char -> unit
      (** Output a single char *)

      method output : bytes -> int -> int -> unit
      (** Output slice *)

      method flush : unit -> unit
      (** Flush underlying buffer *)

      method close : unit -> unit
      (** Close the output. Must be idempotent. *)
    end

    val create :
      ?flush:(unit -> unit) ->
      ?close:(unit -> unit) ->
      output_char:(char -> unit) ->
      output:(bytes -> int -> int -> unit) ->
      unit ->
      t
    (** Create a new output stream from raw components. *)

    val dummy : t
    (** Dummy output, drops everything written to it. *)

    val of_buffer : Buffer.t -> t
    (** [of_buffer buf] is an output channel that writes directly into [buf].
    [flush] and [close] have no effect. *)

    val output_char : #t -> char -> unit
    (** Output a single char *)

    val output : #t -> bytes -> int -> int -> unit
    (** Write the slice of bytes. *)

    val close : #t -> unit
    (** Close the stream. Idempotent. *)

    val flush : #t -> unit
    (** Ensure the bytes written so far are indeed written to the underlying
      storage/network socket/… and are not just sitting in a buffer. *)

    val output_string : #t -> string -> unit
    (** Output the whole string. *)

    val output_line : #t -> string -> unit
    (** Output the whole string followed by ['\n']. *)

    val output_lines : #t -> string Seq.t -> unit
    (** Output a series of lines, each terminated by ['\n']. *)

    val output_int : #t -> int -> unit
    (** Output an integer in decimal notation. *)

    val tee : t list -> t
    (** [tee ocs] is an output that accepts bytes and writes them to every output
    in [ocs]. When closed, it closes all elements of [oc]. *)
  end

  (** Input stream. *)
  module In : sig
    (** An input stream, i.e an incoming stream of bytes.

    This can be a [string], an [int_channel], an [Unix.file_descr], a
    decompression wrapper around another input stream, etc. *)
    class type t = object
      method input : bytes -> int -> int -> int
      (** Read into the slice. Returns [0] only if the
        stream is closed. *)

      method close : unit -> unit
      (** Close the input. Must be idempotent. *)
    end

    val create :
      ?close:(unit -> unit) -> input:(bytes -> int -> int -> int) -> unit -> t

    val empty : t
    (** Empty input, contains 0 bytes. *)

    val of_string : ?off:int -> ?len:int -> string -> t
    (** An input channel reading from the string.
      @param offset initial offset in the string. Default [0].
      @param len the length of the slice we read from. Default [String.length s - off]. *)

    val of_bytes : ?off:int -> ?len:int -> bytes -> t
    (** An input channel reading from the bytes buffer. See {!of_string}
    for more details. *)

    val input : #t -> bytes -> int -> int -> int
    (** Read bytes into the given buffer. This returns [0] only if
      the stream has reached its end.
      @raise Invalid_argument if the arguments do not denote a valid slice. *)

    val input_all : ?buf:bytes -> #t -> string
    (** [input_all ic] reads the whole content of [ic] into a string.
    @param buf the initial buffer to use internally. *)

    val really_input : #t -> bytes -> int -> int -> unit
    (** Same as [input], but reads exactly the demanded amount of bytes.
      @raise Invalid_argument if the arguments do not denote a valid slice.
      @raise End_of_file if the input does not contain enough data. *)

    val really_input_string : #t -> int -> string
    (** [really_input_string ic n] reads exactly [n] bytes of [ic]
      and returns them as a string.
      @raise End_of_file if the input does not contain enough data. *)

    val concat : t list -> t
    (** Read from each stream, in order *)

    val close : #t -> unit
    (** Close the input stream. This is idempotent. *)

    val copy_into : ?buf:bytes -> #t -> Out.t -> unit
    (** Copy the whole stream into the given output. *)
  end
end

(** {2 Sleep} *)

val sleep_s : float -> unit
(** Put the current fiber to sleep for that many seconds. *)

(** {2 Re-exports} *)

exception Timeout
(** Exception used for cancellation caused by timeout *)

val await : 'a Fiber.t -> 'a
(** Wait for the fiber to terminate, and return the result.
    If the fiber failed, this re-raises the exception.

    This must be called from inside another fiber, which will be
    suspended if needed. *)

val try_await : 'a Fiber.t -> 'a Exn_bt.result
(** Like {!await} but catches exceptions. *)

val cancel_after_s : float -> unit
(** Cancel the current fiber after [delay] seconds, unless
    the fiber terminates first. The cancellation will use
    the {!Timeout} exception. *)

val ev_timeout : float -> 'a Event.t
(** [ev_timeout duration] is an event that resolves after
    [duration] seconds with a [Error Timeout] error *)

val ev_deadline : float -> 'a Event.t
(** [ev_deadline time] is an event that resolves at monotonic
    time [t] with a [Error Timeout] error *)

val with_cancel_callback : (Exn_bt.t -> unit) -> (unit -> 'a) -> 'a
(** [let@ () = with_cancel_callback cb in <e>] evaluates [e]
    in a scope in which, if the current fiber is cancelled,
    [cb()] is called. If [e] returns without the fiber being cancelled,
    this callback is removed. *)

val spawn :
  ?name:string -> ?propagate_cancel_to_parent:bool -> (unit -> 'a) -> 'a Fiber.t
(** Must be run from inside the scheduler's thread. Spawn a new computation.
    This fiber has an implicit parent, which is normally the currently running
    fiber (the one calling {!spawn}). If the parent fails or is cancelled, the
    resulting fiber will also be cancelled (parent to child).
    @param propagate_cancel_to_parent if true (the default), if this fiber fails
      then the parent fiber will also fail (child to parent).
    @raise Inactive if the scheduler is inactive. *)

val spawn_from_anywhere :
  ?name:string -> Scheduler.t -> (unit -> 'a) -> 'a Fiber.t
(** Spawn a task from anywhere, possibly from another thread. The task will
    run in a subsequent call to {!run_iteration} in the scheduler's thread.
    Thread-safe, more costly than {!spawn}. Runs under the root switch.
    @raise Inactive if the scheduler is inactive. *)

val spawn_as_child_of :
  ?name:string ->
  ?propagate_cancel_to_parent:bool ->
  Scheduler.t ->
  _ Fiber.t ->
  (unit -> 'a) ->
  'a Fiber.t
(** Spawn a fiber in the given parent fiber's scope.
    See {!spawn} for more details on the arguments *)

val schedule_micro_task : (unit -> unit) -> unit
(** Must be run from inside a {!Scheduler.t}'s thread. Schedules a microtask
    that will run in this tick. Be careful not to create infinite sequences
    of micro tasks that starve the IO loop!

    These microtasks do not handle effects and should try their best to
    not raise exceptions. Only use them for very short amount of work.

    Not thread-safe.
    @raise Inactive if the scheduler is inactive. *)

val yield : unit -> unit
(** [yield ()] returns control to the scheduler and checks for
    cancellation. This must be called from a fiber. *)

val get_scheduler : unit -> Scheduler.t
(** This returns the scheduler on which the caller runs. It must be
    called from inside a fiber, or from inside the thread
    running {!main}.
    @raise Failure if not run from inside a fiber or the thread
    running {!main}. *)

(** {2 Main loop.}

   This is the loop that runs fibers that are ready, and the IO event loop,
   in an interleaved way.
*)

val main : loop:Event_loop.t -> (unit -> 'a) -> 'a
(** [main f] runs [f()] in an event loop. The value is returned
    when the loop has nothing else to do, even if the particular
    computation was finished earlier.

    A high level overview of that [main ~loop f] does, is that
    it has an outer loop in which it does:
      - call the scheduler to run all ready fibers, until no ready fiber remains
        (ready means the fiber isn't suspended while waiting for
        some event to happen)
      - call the event loop without blocking, to poll for IOs
      - if no fiber is ready after polling for IO events, then
          call the event loop and allow it to block/sleep until
          the next event occur (e.g. a timer, or a socket becoming
          readable).

    This only returns when the main fiber running [f()] is
    resolved, either by cancellation or because [f()] returned
    and all child fibers resolved.

    @param loop if provided, this event loop is used
    during the computation. *)

(**/**)

(** Implementation details that should only be used by experts *)
module Private_ : sig
  module TLS = Common_.TLS

  val suspend : before_suspend:(wakeup:(unit -> unit) -> unit) -> unit
end

(**/**)
