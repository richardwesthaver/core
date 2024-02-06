# net.nu

# Permanently allow the 'bandwhich' binary its required privileges with setcap. Requires root.
export def "allow bandwhich" [] {
  sudo setcap cap_sys_ptrace,cap_dac_read_search,cap_net_raw,cap_net_admin+ep ...(which bandwhich).path
}
