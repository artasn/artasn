import os
import subprocess

PATCH_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'patch')

def apply_patches(org: str, modules_dir: str):
    """
    Apply fixes to downloaded modules that, as published, contain errors that prevent ArtASN from parsing them.

    Patches live in patch/<org>/, mirroring the layout of the org's modules directory:
    patch/<org>/[<subdir>/]<ModuleName>.patch is applied to <modules_dir>/[<subdir>/]<ModuleName>.asn.
    Each patch is a unified diff with paths relative to the module's directory (a/<ModuleName>.asn -> b/<ModuleName>.asn).

    This is idempotent: patches that have already been applied are skipped.
    """
    org_patch_dir = os.path.join(PATCH_DIR, org)
    if not os.path.isdir(org_patch_dir):
        return

    patch_paths = []
    for root, _, files in os.walk(org_patch_dir):
        for file in files:
            if file.endswith('.patch'):
                patch_paths.append(os.path.join(root, file))

    for patch_path in sorted(patch_paths):
        patch_name = os.path.relpath(patch_path, org_patch_dir)
        module_dir = os.path.join(modules_dir, os.path.dirname(patch_name))
        module_file = os.path.basename(patch_name)[:-len('.patch')] + '.asn'
        if not os.path.exists(os.path.join(module_dir, module_file)):
            print(f'warning: cannot apply {org}/{patch_name}: {module_file} was not downloaded')
            continue

        def run_patch(*args: str) -> subprocess.CompletedProcess:
            return subprocess.run(
                # --force (rather than --batch) so that patch never auto-detects a "reversed" patch and silently ignores
                # --reverse; --fuzz=0 requires exact context so a patch can't apply to a module that has changed
                ['patch', '-p1', '--force', '--silent', '--fuzz=0', '--no-backup-if-mismatch', '--reject-file=-',
                 '-d', module_dir, '-i', patch_path, *args],
                capture_output=True, text=True,
            )

        if run_patch('--reverse', '--dry-run').returncode == 0:
            print(f'{org}/{patch_name} is already applied')
            continue

        if run_patch('--dry-run').returncode != 0:
            # don't touch the file if the patch doesn't apply cleanly (e.g. the module changed upstream)
            raise RuntimeError(f'{org}/{patch_name} does not apply to {module_file}; the patch may need to be updated')
        run_patch().check_returncode()
        print(f'Applied {org}/{patch_name}')
