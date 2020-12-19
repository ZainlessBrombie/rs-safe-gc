# Safe Rust Garbage collection

_This crate is currently not test-covered and thus considered in alpha. If you'd like to use it for a project, just create a ticket and I will
gladly add plenty of tests._

safe-gc is a crate that provides the garbage collection primitives
`Gc` (akin to an `Rc`) and `GcCell` (the equivalent of a `RefCell`)

It is written entirely in safe rust, so if you need to implement the
`Mark` trait manually and make a mistake, you will at worst encounter a panic,
but no undefined behavior. Using this crate is not a compromise on performance either!

The inspiration for this crate was rust-gc.  
The two main advantages of this crate are that it 1) does not implement
`Drop` on garbage collected types, enabling you to move out of those types
(one of the main reasons I wrote this crate) and implement it yourself if needed
and 2) is written in safe Rust only, which makes it extra safe to use for more critical projects.

###Performance

The obvious comparison would be rust-gc.  
In their simple performance benchmark, this crate  
outperforms rust-gc by a factor of 2.5 for simple allocation-deallocation.
A similar improvement is achieved with a very large tree that is not internally mutable.
A factor of ~1.5 is achieved for retain-all allocation.

In a more intricate worst-case stress test (big looping net), safe-gc was faster by a factor of 1.6.

Also note that GcRefs are still kept on the stack and that Gc is also
reference counted. If a Gc is not internally mutable (does not contain a GcRef), it will
not be considered for marking & root checking, saving performance.

