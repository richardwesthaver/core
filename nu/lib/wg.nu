# wg.nu
export def wg-gen-keys [
  private: string = "private.key"
  public: string = "public.key"
] {
  wg genkey | tee $private | wg pubkey > $public
}
