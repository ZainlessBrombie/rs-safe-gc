# Safe Rust Garbage collection

_☝️ This crate is a proof of concept. I'm archiving this but leaving this up in case anyone looks for inspiration on how it works._  
_Also, this crate is quite old._


safe-gc is a crate that provides the garbage collection primitives
`Gc` (akin to an `Rc`) and `GcCell` (the equivalent of a `RefCell`)

It is written entirely in safe rust, so the
`Mark` trait needs to be implemented manually and a mistake is made, a panic may result,
but no undefined behavior. In my limited testing, the performance of this crate was comparable to that of another, somewhat popular crate.

The inspiration for this crate was rust-gc.  
The two main advantages of this crate are that it 1) does not implement
`Drop` on garbage collected types, making it possible to move out of those types
(one of the main reasons I wrote this crate) and manually implement Drop if needed
and 2) is written in safe Rust only, which would make it more safe to use for projects with more safety concerns.

### Performance
_These metrics may be way out of date_
The obvious comparison would be rust-gc.  
In their simple performance benchmark, this crate  
outperforms rust-gc by a factor of 2.5 for simple allocation-deallocation.
A similar improvement is achieved with a very large tree that is not internally mutable.
A factor of ~1.5 is achieved for retain-all allocation.

In a more intricate worst-case stress test (big looping net), safe-gc was faster by a factor of 1.6.

Also note that GcRefs are still kept on the stack and that Gc is also
reference counted. If a Gc is not internally mutable (does not contain a GcRef), it will
not be considered for marking & root checking, saving performance.

However these tests do not test real world conditions nor worst cases. Again, this crate is an experiment.
