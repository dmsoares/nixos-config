# Debugging suspend hangs (Tuxedo InfinityBook Pro Gen10)

This machine only supports s2idle (no S3/deep sleep). Suspend hangs have been observed where the system fails to wake and requires a hard power-off.

## After a hang and hard reboot

### 1. Read the AMD Smart Trace Buffer

The `amd_pmc.enable_stb=1` kernel parameter is enabled. After a hang+reboot, the STB from the failed suspend is available:

```bash
sudo cat /sys/kernel/debug/amd_pmc/stb_read > /tmp/stb_dump.txt
```

Save this before suspending again — it gets overwritten on the next suspend cycle.

### 2. Check suspend/resume logs from the previous boot

```bash
# Full suspend/resume timeline
journalctl -b -1 --no-pager | grep -iE "suspend|resume|sleep|wake|PM:"

# ACPI errors
journalctl -b -1 --no-pager -k | grep -iE "ACPI.*error|ucsi|amd_pmc"

# tccd-sleep service (should no longer fail after the fix)
journalctl -b -1 --no-pager -u tccd-sleep
```

### 3. Check s0ix residency

If the system wakes but battery drained heavily, s0ix may not be reached:

```bash
sudo cat /sys/kernel/debug/amd_pmc/s0ix_stats
```

A `Success` count of 0 with a non-zero `Attempt` count means the SoC never entered deep idle.

## Next steps if hangs persist

Try these in order, rebuilding and testing after each:

1. **`acpi_osi` parameter** — prevents the BIOS from using broken Windows 11 sleep paths:
   ```nix
   # in machines/tuxedo-laptop/configuration.nix, append to boot.kernelParams:
   boot.kernelParams = [ "amd_pmc.enable_stb=1" ''acpi_osi="!Windows 2022"'' ];
   ```
   Caveat: may affect fan control or brightness. Revert if new issues appear.

2. **BIOS update** — the ACPI errors (`_SB.PEP._DSM`, `NPCF`) are firmware bugs. Check your Tuxedo customer account at https://www.tuxedocomputers.com/en/Downloads-Drivers.tuxedo for newer BIOS/EC firmware.

3. **Kernel version** — if the issue started with a specific kernel, pin to an older one temporarily:
   ```nix
   boot.kernelPackages = pkgs.linuxPackages_6_12;  # or another known-good version
   ```

## Current mitigations applied

All in `machines/tuxedo-laptop/configuration.nix`:

- `boot.blacklistedKernelModules = [ "ucsi_acpi" ]` — prevents USB-C controller timeout on resume
- `boot.kernelParams = [ "amd_pmc.enable_stb=1" ]` — enables suspend diagnostics
- `systemd.services.tccd-sleep.serviceConfig` override — fixes broken TCC sleep/resume service
