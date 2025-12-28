("/opt/cuda/include/cuda.h"
 "/opt/cuda/include/cufft.h"
 "/opt/cuda/include/cufftw.h"
 ;; "/opt/cuda/include/cublas_api.h" ;; groveler can't handle it!
 "/usr/include/cudnn_version.h")
                            
(;; cuda
 (:integer +cuda-version+ "CUDA_VERSION" nil t)
 (:integer +cu-ipc-handle-size+ "CU_IPC_HANDLE_SIZE" nil t)
 (:integer +cu-compute-accelerated-target-base+ "CU_COMPUTE_ACCELERATED_TARGET_BASE" nil t)
 (:integer +cu-graph-cond-assign-default+ "CU_GRAPH_COND_ASSIGN_DEFAULT" nil t)
 (:integer +cu-graph-kernel-node-port-default+ "CU_GRAPH_KERNEL_NODE_PORT_DEFAULT" nil t)
 (:integer +cu-graph-kernel-node-port-launch-order+ "CU_GRAPH_KERNEL_NODE_PORT_LAUNCH_ORDER" nil t)
 (:integer +cu-graph-kernel-node-port-programmatic+ "CU_GRAPH_KERNEL_NODE_PORT_PROGRAMMATIC" nil t)
 (:integer +cu-memhostalloc-portable+ "CU_MEMHOSTALLOC_PORTABLE" nil t)
 (:integer +cu-memhostalloc-devicemap+ "CU_MEMHOSTALLOC_DEVICEMAP" nil t)
 (:integer +cu-memhostalloc-writecombined+ "CU_MEMHOSTALLOC_WRITECOMBINED" nil t)
 (:integer +cu-memhostregister-portable+ "CU_MEMHOSTREGISTER_PORTABLE" nil t)
 (:integer +cu-memhostregister-devicemap+ "CU_MEMHOSTREGISTER_DEVICEMAP" nil t)
 (:integer +cu-memhostregister-iomemory+ "CU_MEMHOSTREGISTER_IOMEMORY" nil t)
 (:integer +cu-memhostregister-read-only+ "CU_MEMHOSTREGISTER_READ_ONLY" nil t)
 (:integer +cu-array-sparse-properties-single-miptail+ "CU_ARRAY_SPARSE_PROPERTIES_SINGLE_MIPTAIL" nil t)
 (:integer +cu-tensor-map-num-qwords+ "CU_TENSOR_MAP_NUM_QWORDS" nil t)
 ;; cufft
 (:integer +cufft-ver-major+ "CUFFT_VER_MAJOR" nil t)
 (:integer +cufft-ver-minor+ "CUFFT_VER_MINOR" nil t)
 (:integer +cufft-ver-patch+ "CUFFT_VER_PATCH" nil t)
 (:integer +cufft-ver-build+ "CUFFT_VER_BUILD" nil t)
 (:integer +max-cufft-error+ "MAX_CUFFT_ERROR" nil t)
 (:integer +cufft-forward+ "CUFFT_FORWARD" nil t)
 (:integer +cufft-inverse+ "CUFFT_INVERSE" nil t)
 (:integer +max-shim-rank+ "MAX_SHIM_RANK" nil t)
 (:integer +cufft-plan-null+ "CUFFT_PLAN_NULL" nil t)
 ;; cufftw
 (:integer +fftw-forward+ "FFTW_FORWARD" nil t)
 (:integer +fftw-inverse+ "FFTW_INVERSE" nil t)
 (:integer +fftw-backward+ "FFTW_BACKWARD" nil t)
 (:integer +fftw-estimate+ "FFTW_ESTIMATE" nil t)
 (:integer +fftw-measure+ "FFTW_MEASURE" nil t)
 (:integer +fftw-patient+ "FFTW_PATIENT" nil t)
 (:integer +fftw-exhaustive+ "FFTW_EXHAUSTIVE" nil t)
 (:integer +fftw-wisdom-only+ "FFTW_WISDOM_ONLY" nil t)
 (:integer +fftw-destroy-input+ "FFTW_DESTROY_INPUT" nil t)
 (:integer +fftw-preserve-input+ "FFTW_PRESERVE_INPUT" nil t)
 (:integer +fftw-unaligned+ "FFTW_UNALIGNED" nil t)
 ;; TODO 2025-12-19: 
 ;; cublas
 ;; (:integer +cublas-ver-major+ "CUBLAS_VER_MAJOR" nil t)
 ;; (:integer +cublas-ver-minor+ "CUBLAS_VER_MINOR" nil t)
 ;; (:integer +cublas-ver-patch+ "CUBLAS_VER_PATCH" nil t)
 ;; (:integer +cublas-ver-build+ "CUBLAS_VER_BUILD" nil t)
 ;; cudnn
 (:integer +cudnn-major+ "CUDNN_MAJOR" nil t)
 (:integer +cudnn-minor+ "CUDNN_MINOR" nil t)
 (:integer +cudnn-patchlevel+ "CUDNN_PATCHLEVEL" nil t)
 (:integer +cudnn-version+ "CUDNN_VERSION" nil t)
 (:integer +cudnn-max-sm-major-number+ "CUDNN_MAX_SM_MAJOR_NUMBER" nil t)
 (:integer +cudnn-max-sm-minor-number+ "CUDNN_MAX_SM_MINOR_NUMBER" nil t)
 (:integer +cudnn-max-device-version+ "CUDNN_MAX_DEVICE_VERSION" nil t))

