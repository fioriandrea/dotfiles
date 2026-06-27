install-dotfiles
=========

Installs a dotfiles Git repository using `rsync`.

Requirements
------------

Installs `git` and `rsync` on the target host.

The target host must be able to access `dotfiles_repo_url`.

Role Variables
--------------

See `defaults/main.yml` for default values.

Configurable variables:

```yaml
dotfiles_repo_url:    # Git repository URL
dotfiles_version:     # Branch, tag, or commit
dotfiles_install_dir: # Destination directory; defaults to ansible_env.HOME when unset
dotfiles_backup_dir:  # Optional backup directory for overwritten files
```

If `dotfiles_backup_dir` is set, overwritten files are backed up there. Otherwise, `rsync --backup` uses its default behavior.

Dependencies
------------

None.

Example Playbook
----------------

```yaml
- hosts: workstations
  roles:
    - role: dotfiles
```

Example with overrides:

```yaml
- hosts: workstations
  roles:
    - role: dotfiles
      vars:
        dotfiles_repo_url: "git@github.com:username/dotfiles.git"
        dotfiles_version: "main"
```

Example with backup directory:

```yaml
- hosts: workstations
  roles:
    - role: dotfiles
      vars:
        dotfiles_backup_dir: "{{ ansible_env.HOME }}/.dotfiles-backup"
```

License
-------

GPLv3+

Author Information
------------------

https://github.com/fioriandrea
