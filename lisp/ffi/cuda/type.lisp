;;; type.lisp --- CUDA Types

;; CUDA Base Types

;;; Code:
(in-package :cuda)

(define-alien-type cu-device int)
(define-alien-type cu-context (* t))
(define-alien-type cu-module (* t))
(define-alien-type cu-function (* t))
(define-alien-type cu-stream (* t))
(define-alien-type cu-event (* t))
(define-alien-type cu-graphics-resource (* t))
(define-alien-type cu-device-ptr unsigned-long-long)

(define-alien-enum (cu-event-flag int)
                   :default 0
                   :blocking-sync 1
                   :disable-timing 2
                   :interprocess 4)

(define-alien-enum (cu-stream-flag int)
                   :default 0
                   :non-blocking 1)

(define-alien-enum (cu-memhostalloc-flag int)
                   :portable 1
                   :devicemap 2
                   :writecombined 4)

(define-alien-enum (cu-memhostregister-flag int)
                   :portable 1
                   :devicemap 2
                   :iomemory 4
                   :read-only 8)

(define-alien-enum 
    (cu-result int)
    ;; The API call returned with no errors. In the case of query calls, this
    ;; also means that the operation being queried is complete (see
    ;; ::cuEventQuery() and ::cuStreamQuery()).
    :success 0
    #|
     * This indicates that one or more of the parameters passed to the API call
     * is not within an acceptable range of values.
    |#
    :error-invalid-value 1

    #|
     * The API call failed because it was unable to allocate enough memory or
     * other resources to perform the requested operation.
    |#
    :error-out-of-memory 2

    #|
     * This indicates that the CUDA driver has not been initialized with
     * ::cuInit() or that initialization has failed.
    |#
    :error-not-initialized 3

    #|
     * This indicates that the CUDA driver is in the process of shutting down.
    |#
    :error-deinitialized 4

    #|
     * This indicates profiler is not initialized for this run. This can
     * happen when the application is running with external profiling tools
     * like visual profiler.
    |#
    :error-profiler-disabled 5

    #|
     * \deprecated
     * This error return is deprecated as of CUDA 5.0. It is no longer an error
     * to attempt to enable/disable the profiling via ::cuProfilerStart or
     * ::cuProfilerStop without initialization.
    |#
    :error-profiler-not-initialized 6

    #|
     * \deprecated
     * This error return is deprecated as of CUDA 5.0. It is no longer an error
     * to call cuProfilerStart() when profiling is already enabled.
    |#
    :error-profiler-already-started 7

    #|
     * \deprecated
     * This error return is deprecated as of CUDA 5.0. It is no longer an error
     * to call cuProfilerStop() when profiling is already disabled.
    |#
    :error-profiler-already-stopped 8

    #|
     * This indicates that the CUDA driver that the application has loaded is a
     * stub library. Applications that run with the stub rather than a real
     * driver loaded will result in CUDA API returning this error.
    |#
    :error-stub-library 34

    #|
     * This indicates that requested CUDA device is unavailable at the current
     * time. Devices are often unavailable due to use of
     * ::CU_COMPUTEMODE_EXCLUSIVE_PROCESS or ::CU_COMPUTEMODE_PROHIBITED.
    |#
    :error-device-unavailable 46

    #|
     * This indicates that no CUDA-capable devices were detected by the installed
     * CUDA driver.
    |#
    :error-no-device 100

    #|
     * This indicates that the device ordinal supplied by the user does not
     * correspond to a valid CUDA device or that the action requested is
     * invalid for the specified device.
    |#
    :error-invalid-device 101

    #|
     * This error indicates that the Grid license is not applied.
    |#
    :error-device-not-licensed 102

    #|
     * This indicates that the device kernel image is invalid. This can also
     * indicate an invalid CUDA module.
    |#
    :error-invalid-image 200

    #|
     * This most frequently indicates that there is no context bound to the
     * current thread. This can also be returned if the context passed to an
     * API call is not a valid handle (such as a context that has had
     * ::cuCtxDestroy() invoked on it). This can also be returned if a user
     * mixes different API versions (i.e. 3010 context with 3020 API calls).
     * See ::cuCtxGetApiVersion() for more details.
     * This can also be returned if the green context passed to an API call
     * was not converted to a ::CUcontext using ::cuCtxFromGreenCtx API.
    |#
    :error-invalid-context 201

    #|
     * This indicated that the context being supplied as a parameter to the
     * API call was already the active context.
     * \deprecated
     * This error return is deprecated as of CUDA 3.2. It is no longer an
     * error to attempt to push the active context via ::cuCtxPushCurrent().
    |#
    :error-context-already-current 202

    #|
     * This indicates that a map or register operation has failed.
    |#
    :error-map-failed 205

    #|
     * This indicates that an unmap or unregister operation has failed.
    |#
    :error-unmap-failed 206

    #|
     * This indicates that the specified array is currently mapped and thus
     * cannot be destroyed.
    |#
    :error-array-is-mapped 207

    #|
     * This indicates that the resource is already mapped.
    |#
    :error-already-mapped 208

    #|
     * This indicates that there is no kernel image available that is suitable
     * for the device. This can occur when a user specifies code generation
     * options for a particular CUDA source file that do not include the
     * corresponding device configuration.
    |#
    :error-no-binary-for-gpu 209

    #|
     * This indicates that a resource has already been acquired.
    |#
    :error-already-acquired 210

    #|
     * This indicates that a resource is not mapped.
    |#
    :error-not-mapped 211

    #|
     * This indicates that a mapped resource is not available for access as an
     * array.
    |#
    :error-not-mapped-as-array 212

    #|
     * This indicates that a mapped resource is not available for access as a
     * pointer.
    |#
    :error-not-mapped-as-pointer 213

    #|
     * This indicates that an uncorrectable ECC error was detected during
     * execution.
    |#
    :error-ecc-uncorrectable 214

    #|
     * This indicates that the ::CUlimit passed to the API call is not
     * supported by the active device.
    |#
    :error-unsupported-limit 215

    #|
     * This indicates that the ::CUcontext passed to the API call can
     * only be bound to a single CPU thread at a time but is already
     * bound to a CPU thread.
    |#
    :error-context-already-in-use 216

    #|
     * This indicates that peer access is not supported across the given
     * devices.
    |#
    :error-peer-access-unsupported 217

    #|
     * This indicates that a PTX JIT compilation failed.
    |#
    :error-invalid-ptx 218

    #|
     * This indicates an error with OpenGL or DirectX context.
    |#
    :error-invalid-graphics-context 219

    #|
    * This indicates that an uncorrectable NVLink error was detected during the
    * execution.
    |#
    :error-nvlink-uncorrectable 220

    #|
    * This indicates that the PTX JIT compiler library was not found.
    |#
    :error-jit-compiler-not-found 221

    #|
     * This indicates that the provided PTX was compiled with an unsupported toolchain.
    |#
    :error-unsupported-ptx-version 222

    #|
     * This indicates that the PTX JIT compilation was disabled.
    |#
    :error-jit-compilation-disabled 223

    #|
     * This indicates that the ::CUexecAffinityType passed to the API call is not
     * supported by the active device.
    |#
    :error-unsupported-exec-affinity 224

    #|
     * This indicates that the code to be compiled by the PTX JIT contains
     * unsupported call to cudaDeviceSynchronize.
    |#
    :error-unsupported-devside-sync 225

    #|
     * This indicates that the device kernel source is invalid. This includes
     * compilation/linker errors encountered in device code or user error.
    |#
    :error-invalid-source 300

    #|
     * This indicates that the file specified was not found.
    |#
    :error-file-not-found 301

    #|
     * This indicates that a link to a shared object failed to resolve.
    |#
    :error-shared-object-symbol-not-found 302

    #|
     * This indicates that initialization of a shared object failed.
    |#
    :error-shared-object-init-failed 303

    #|
     * This indicates that an OS call failed.
    |#
    :error-operating-system 304

    #|
     * This indicates that a resource handle passed to the API call was not
     * valid. Resource handles are opaque types like ::CUstream and ::CUevent.
    |#
    :error-invalid-handle 400

    #|
     * This indicates that a resource required by the API call is not in a
     * valid state to perform the requested operation.
    |#
    :error-illegal-state 401

    #|
     * This indicates an attempt was made to introspect an object in a way that
     * would discard semantically important information. This is either due to
     * the object using funtionality newer than the API version used to
     * introspect it or omission of optional return arguments.
    |#
    :error-lossy-query 402

    #|
     * This indicates that a named symbol was not found. Examples of symbols
     * are global/constant variable names, driver function names, texture names,
     * and surface names.
    |#
    :error-not-found 500

    #|
     * This indicates that asynchronous operations issued previously have not
     * completed yet. This result is not actually an error, but must be indicated
     * differently than ::CUDA_SUCCESS (which indicates completion). Calls that
     * may return this value include ::cuEventQuery() and ::cuStreamQuery().
    |#
    :error-not-ready 600

    #|
     * While executing a kernel, the device encountered a
     * load or store instruction on an invalid memory address.
     * This leaves the process in an inconsistent state and any further CUDA work
     * will return the same error. To continue using CUDA, the process must be terminated
     * and relaunched.
    |#
    :error-illegal-address 700

    #|
     * This indicates that a launch did not occur because it did not have
     * appropriate resources. This error usually indicates that the user has
     * attempted to pass too many arguments to the device kernel, or the
     * kernel launch specifies too many threads for the kernel's register
     * count. Passing arguments of the wrong size (i.e. a 64-bit pointer
     * when a 32-bit int is expected) is equivalent to passing too many
     * arguments and can also result in this error.
    |#
    :error-launch-out-of-resources 701

    #|
     * This indicates that the device kernel took too long to execute. This can
     * only occur if timeouts are enabled - see the device attribute
     * ::CU_DEVICE_ATTRIBUTE_KERNEL_EXEC_TIMEOUT for more information.
     * This leaves the process in an inconsistent state and any further CUDA work
     * will return the same error. To continue using CUDA, the process must be terminated
     * and relaunched.
    |#
    :error-launch-timeout 702

    #|
     * This error indicates a kernel launch that uses an incompatible texturing
     * mode.
    |#
    :error-launch-incompatible-texturing 703

    #|
     * This error indicates that a call to ::cuCtxEnablePeerAccess() is
     * trying to re-enable peer access to a context which has already
     * had peer access to it enabled.
    |#
    :error-peer-access-already-enabled 704

    #|
     * This error indicates that ::cuCtxDisablePeerAccess() is
     * trying to disable peer access which has not been enabled yet
     * via ::cuCtxEnablePeerAccess().
    |#
    :error-peer-access-not-enabled 705

    #|
     * This error indicates that the primary context for the specified device
     * has already been initialized.
    |#
    :error-primary-context-active 708

    #|
     * This error indicates that the context current to the calling thread
     * has been destroyed using ::cuCtxDestroy, or is a primary context which
     * has not yet been initialized.
    |#
    :error-context-is-destroyed 709

    #|
     * A device-side assert triggered during kernel execution. The context
     * cannot be used anymore, and must be destroyed. All existing device
     * memory allocations from this context are invalid and must be
     * reconstructed if the program is to continue using CUDA.
    |#
    :error-assert 710

    #|
     * This error indicates that the hardware resources required to enable
     * peer access have been exhausted for one or more of the devices
     * passed to ::cuCtxEnablePeerAccess().
    |#
    :error-too-many-peers 711

    #|
     * This error indicates that the memory range passed to ::cuMemHostRegister()
     * has already been registered.
    |#
    :error-host-memory-already-registered 712

    #|
     * This error indicates that the pointer passed to ::cuMemHostUnregister()
     * does not correspond to any currently registered memory region.
    |#
    :error-host-memory-not-registered 713

    #|
     * While executing a kernel, the device encountered a stack error.
     * This can be due to stack corruption or exceeding the stack size limit.
     * This leaves the process in an inconsistent state and any further CUDA work
     * will return the same error. To continue using CUDA, the process must be terminated
     * and relaunched.
    |#
    :error-hardware-stack-error 714

    #|
     * While executing a kernel, the device encountered an illegal instruction.
     * This leaves the process in an inconsistent state and any further CUDA work
     * will return the same error. To continue using CUDA, the process must be terminated
     * and relaunched.
    |#
    :error-illegal-instruction 715

    #|
     * While executing a kernel, the device encountered a load or store instruction
     * on a memory address which is not aligned.
     * This leaves the process in an inconsistent state and any further CUDA work
     * will return the same error. To continue using CUDA, the process must be terminated
     * and relaunched.
    |#
    :error-misaligned-address 716

    #|
     * While executing a kernel, the device encountered an instruction
     * which can only operate on memory locations in certain address spaces
     * (global, shared, or local), but was supplied a memory address not
     * belonging to an allowed address space.
     * This leaves the process in an inconsistent state and any further CUDA work
     * will return the same error. To continue using CUDA, the process must be terminated
     * and relaunched.
    |#
    :error-invalid-address-space 717

    #|
     * While executing a kernel, the device program counter wrapped its address space.
     * This leaves the process in an inconsistent state and any further CUDA work
     * will return the same error. To continue using CUDA, the process must be terminated
     * and relaunched.
    |#
    :error-invalid-pc 718

    #|
     * An exception occurred on the device while executing a kernel. Common
     * causes include dereferencing an invalid device pointer and accessing
     * out of bounds shared memory. Less common cases can be system specific - more
     * information about these cases can be found in the system specific user guide.
     * This leaves the process in an inconsistent state and any further CUDA work
     * will return the same error. To continue using CUDA, the process must be terminated
     * and relaunched.
    |#
    :error-launch-failed 719

    #|
     * This error indicates that the number of blocks launched per grid for a kernel that was
     * launched via either ::cuLaunchCooperativeKernel or ::cuLaunchCooperativeKernelMultiDevice
     * exceeds the maximum number of blocks as allowed by ::cuOccupancyMaxActiveBlocksPerMultiprocessor
     * or ::cuOccupancyMaxActiveBlocksPerMultiprocessorWithFlags times the number of multiprocessors
     * as specified by the device attribute ::CU_DEVICE_ATTRIBUTE_MULTIPROCESSOR_COUNT.
    |#
    :error-cooperative-launch-too-large 720

    #|
     * This error indicates that the attempted operation is not permitted.
    |#
    :error-not-permitted 800

    #|
     * This error indicates that the attempted operation is not supported
     * on the current system or device.
    |#
    :error-not-supported 801

    #|
     * This error indicates that the system is not yet ready to start any CUDA
     * work.  To continue using CUDA, verify the system configuration is in a
     * valid state and all required driver daemons are actively running.
     * More information about this error can be found in the system specific
     * user guide.
    |#
    :error-system-not-ready 802

    #|
     * This error indicates that there is a mismatch between the versions of
     * the display driver and the CUDA driver. Refer to the compatibility documentation
     * for supported versions.
    |#
    :error-system-driver-mismatch 803

    #|
     * This error indicates that the system was upgraded to run with forward compatibility
     * but the visible hardware detected by CUDA does not support this configuration.
     * Refer to the compatibility documentation for the supported hardware matrix or ensure
     * that only supported hardware is visible during initialization via the CUDA_VISIBLE_DEVICES
     * environment variable.
    |#
    :error-compat-not-supported-on-device 804

    #|
     * This error indicates that the MPS client failed to connect to the MPS control daemon or the MPS server.
    |#
    :error-mps-connection-failed 805

    #|
     * This error indicates that the remote procedural call between the MPS server and the MPS client failed.
    |#
    :error-mps-rpc-failure 806

    #|
     * This error indicates that the MPS server is not ready to accept new MPS client requests.
     * This error can be returned when the MPS server is in the process of recovering from a fatal failure.
    |#
    :error-mps-server-not-ready 807

    #|
     * This error indicates that the hardware resources required to create MPS client have been exhausted.
    |#
    :error-mps-max-clients-reached 808

    #|
     * This error indicates the the hardware resources required to support device connections have been exhausted.
    |#
    :error-mps-max-connections-reached 809

    #|
     * This error indicates that the MPS client has been terminated by the server. To continue using CUDA, the process must be terminated and relaunched.
    |#
    :error-mps-client-terminated 810

    #|
     * This error indicates that the module is using CUDA Dynamic Parallelism, but the current configuration, like MPS, does not support it.
    |#
    :error-cdp-not-supported 811

    #|
     * This error indicates that a module contains an unsupported interaction between different versions of CUDA Dynamic Parallelism.
    |#
    :error-cdp-version-mismatch 812

    #|
     * This error indicates that the operation is not permitted when
     * the stream is capturing.
    |#
    :error-stream-capture-unsupported 900

    #|
     * This error indicates that the current capture sequence on the stream
     * has been invalidated due to a previous error.
    |#
    :error-stream-capture-invalidated 901

    #|
     * This error indicates that the operation would have resulted in a merge
     * of two independent capture sequences.
    |#
    :error-stream-capture-merge 902

    #|
     * This error indicates that the capture was not initiated in this stream.
    |#
    :error-stream-capture-unmatched 903

    #|
     * This error indicates that the capture sequence contains a fork that was
     * not joined to the primary stream.
    |#
    :error-stream-capture-unjoined 904

    #|
     * This error indicates that a dependency would have been created which
     * crosses the capture sequence boundary. Only implicit in-stream ordering
     * dependencies are allowed to cross the boundary.
    |#
    :error-stream-capture-isolation 905

    #|
     * This error indicates a disallowed implicit dependency on a current capture
     * sequence from cudaStreamLegacy.
    |#
    :error-stream-capture-implicit 906

    #|
     * This error indicates that the operation is not permitted on an event which
     * was last recorded in a capturing stream.
    |#
    :error-captured-event 907

    #|
     * A stream capture sequence not initiated with the ::CU_STREAM_CAPTURE_MODE_RELAXED
     * argument to ::cuStreamBeginCapture was passed to ::cuStreamEndCapture in a
     * different thread.
    |#
    :error-stream-capture-wrong-thread 908

    #|
     * This error indicates that the timeout specified for the wait operation has lapsed.
    |#
    :error-timeout 909

    #|
     * This error indicates that the graph update was not performed because it included 
     * changes which violated constraints specific to instantiated graph update.
    |#
    :error-graph-exec-update-failure 910

    #|
     * This indicates that an async error has occurred in a device outside of CUDA.
     * If CUDA was waiting for an external device's signal before consuming shared data,
     * the external device signaled an error indicating that the data is not valid for
     * consumption. This leaves the process in an inconsistent state and any further CUDA
     * work will return the same error. To continue using CUDA, the process must be
     * terminated and relaunched.
    |#
    :error-external-device 911

    #|
     * Indicates a kernel launch error due to cluster misconfiguration.
    |#
    :error-invalid-cluster-size 912

    #|
     * Indiciates a function handle is not loaded when calling an API that requires
     * a loaded function.
    |#
    :error-function-not-loaded 913

    #|
     * This error indicates one or more resources passed in are not valid resource
     * types for the operation.
    |#
    :error-invalid-resource-type 914

    #|
     * This error indicates one or more resources are insufficient or non-applicable for
     * the operation.
    |#
    :error-invalid-resource-configuration 915

    #|
     * This indicates that an unknown internal error has occurred.
    |#
    :error-unknown 999)
