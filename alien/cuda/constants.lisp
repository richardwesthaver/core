("/opt/cuda/include/cuda.h"
 "/opt/cuda/include/cufft.h"
 "/opt/cuda/include/cufftw.h"
 ;; "/opt/cuda/include/cublas_api.h" ;; groveler can't handle it!
 "/usr/include/cudnn_version.h")
                            
(;; cuda
 (:integer +cuda-version+ "CUDA_VERSION")
 (:integer +cu-ipc-handle-size+ "CU_IPC_HANDLE_SIZE")
 (:integer +cu-compute-accelerated-target-base+ "CU_COMPUTE_ACCELERATED_TARGET_BASE")
 (:integer +cu-graph-cond-assign-default+ "CU_GRAPH_COND_ASSIGN_DEFAULT")
 (:integer +cu-graph-kernel-node-port-default+ "CU_GRAPH_KERNEL_NODE_PORT_DEFAULT")
 (:integer +cu-graph-kernel-node-port-launch-order+ "CU_GRAPH_KERNEL_NODE_PORT_LAUNCH_ORDER")
 (:integer +cu-graph-kernel-node-port-programmatic+ "CU_GRAPH_KERNEL_NODE_PORT_PROGRAMMATIC")
 (:integer +cu-memhostalloc-portable+ "CU_MEMHOSTALLOC_PORTABLE")
 (:integer +cu-memhostalloc-devicemap+ "CU_MEMHOSTALLOC_DEVICEMAP")
 (:integer +cu-memhostalloc-writecombined+ "CU_MEMHOSTALLOC_WRITECOMBINED")
 (:integer +cu-memhostregister-portable+ "CU_MEMHOSTREGISTER_PORTABLE")
 (:integer +cu-memhostregister-devicemap+ "CU_MEMHOSTREGISTER_DEVICEMAP")
 (:integer +cu-memhostregister-iomemory+ "CU_MEMHOSTREGISTER_IOMEMORY")
 (:integer +cu-memhostregister-read-only+ "CU_MEMHOSTREGISTER_READ_ONLY")
 (:integer +cu-array-sparse-properties-single-miptail+ "CU_ARRAY_SPARSE_PROPERTIES_SINGLE_MIPTAIL")
 (:integer +cu-tensor-map-num-qwords+ "CU_TENSOR_MAP_NUM_QWORDS")
 ;; cufft
 (:integer +cufft-ver-major+ "CUFFT_VER_MAJOR")
 (:integer +cufft-ver-minor+ "CUFFT_VER_MINOR")
 (:integer +cufft-ver-patch+ "CUFFT_VER_PATCH")
 (:integer +cufft-ver-build+ "CUFFT_VER_BUILD")
 (:integer +max-cufft-error+ "MAX_CUFFT_ERROR")
 (:integer +cufft-forward+ "CUFFT_FORWARD")
 (:integer +cufft-inverse+ "CUFFT_INVERSE")
 (:integer +max-shim-rank+ "MAX_SHIM_RANK")
 (:integer +cufft-plan-null+ "CUFFT_PLAN_NULL")
 ;; cufftw
 (:integer +fftw-forward+ "FFTW_FORWARD")
 (:integer +fftw-inverse+ "FFTW_INVERSE")
 (:integer +fftw-backward+ "FFTW_BACKWARD")
 (:integer +fftw-estimate+ "FFTW_ESTIMATE")
 (:integer +fftw-measure+ "FFTW_MEASURE")
 (:integer +fftw-patient+ "FFTW_PATIENT")
 (:integer +fftw-exhaustive+ "FFTW_EXHAUSTIVE")
 (:integer +fftw-wisdom-only+ "FFTW_WISDOM_ONLY")
 (:integer +fftw-destroy-input+ "FFTW_DESTROY_INPUT")
 (:integer +fftw-preserve-input+ "FFTW_PRESERVE_INPUT")
 (:integer +fftw-unaligned+ "FFTW_UNALIGNED")
 ;; TODO 2025-12-19: 
 ;; cublas
 ;; (:integer +cublas-ver-major+ "CUBLAS_VER_MAJOR")
 ;; (:integer +cublas-ver-minor+ "CUBLAS_VER_MINOR")
 ;; (:integer +cublas-ver-patch+ "CUBLAS_VER_PATCH")
 ;; (:integer +cublas-ver-build+ "CUBLAS_VER_BUILD")
 ;; cudnn
 (:integer +cudnn-major+ "CUDNN_MAJOR")
 (:integer +cudnn-minor+ "CUDNN_MINOR")
 (:integer +cudnn-patchlevel+ "CUDNN_PATCHLEVEL")
 (:integer +cudnn-version+ "CUDNN_VERSION")
 (:integer +cudnn-max-sm-major-number+ "CUDNN_MAX_SM_MAJOR_NUMBER")
 (:integer +cudnn-max-sm-minor-number+ "CUDNN_MAX_SM_MINOR_NUMBER")
 (:integer +cudnn-max-device-version+ "CUDNN_MAX_DEVICE_VERSION"))

