# Floating context experiment

This feature branch targets **Emacs 31.1+**. It replaces XOB's context split with
three independently toggleable child frames: backlinks, forward links, and a
preview. Typing stays in the XOB editor. No additional editor windows are opened.

## Try it

Use `feature/child-frame-context` from `vv111y/org-xob`, with your existing XOB
dependencies. Install `vv111y/emacs-tui` at the tested beta 7 commit
`fa186fe11f565fa6d41084ab4c2079728ebfe9a2`. Both repositories must be on `load-path`.
For example, with straight.el:

```elisp
(straight-use-package
 '(emacs-tui :type git :host github :repo "vv111y/emacs-tui"
             :branch "release/0.1.0-beta7"
             :files ("emacs-tui*.el")))
(straight-use-package
 '(org-xob :type git :host github :repo "vv111y/org-xob"
           :branch "feature/child-frame-context"
           :files ("org-xob*.el")))

(require 'org-xob)
(require 'org-xob-tui)
(org-xob-tui-mode 1)
```

If XOB is already installed, update its existing package recipe and checkout
instead of loading a second copy. Restart Emacs after changing checkouts.
Enable the mode before opening a node, then use your usual `org-xob-start` and
`org-xob-get-node` workflow. Enabling the mode does not start or scan a repository.
It can also be enabled in an existing session; it removes the old paired context
window when its editor is still visible.

In an XOB editing buffer, press **`C-c x 0`** to show or hide all panels.
The normal automatic backlink/forward-link commands also use child frames while
this mode is enabled. Native child-frame support is required; unsupported
displays report an error before changing the layout.

## Controls

These keys are local to XOB editing buffers. The initially active panel is the
forward-links list; use `C-c x TAB` to choose another visible panel.

| Key | Action |
| --- | --- |
| `C-c x 1` / `2` / `3` | Toggle backlinks / forward links / preview |
| `C-c x 0` | Toggle all panels |
| `C-c x TAB` | Cycle the active visible panel |
| `C-c x n` / `p` | Next / previous link; scroll when the preview is active |
| `C-c x d` | Pin or release the preview |
| `C-c x m` | Move the active panel through eight anchor positions |
| `C-c x e` | Enlarge or shrink the active panel |
| `C-c x g` | Refresh visible context |
| `C-c x RET` | Open the selected node through XOB's editing workflow |
| `C-c x q` | Hide all panels |

Selections use node IDs, so identical titles remain separate entries. Browsing
only updates a visible preview; it does not reopen a preview you have hidden.
Hiding and reopening panels preserves their size, placement, and selections.
A pinned preview stays fixed while browsing links; moving to another source node
resets the selection and pin. Leaving or repurposing the source window closes
its child frames. The next toggle reopens them for the current source.

## Presentation and source data

Previews use read-only presentation buffers, not Org editing buffers. Property
drawers, other drawers, planning lines, and Org keywords are omitted. Headings
lose their stars; links show labels; emphasis uses faces. Source blocks display
as literal text. Nothing is exported or evaluated, and the underlying Org files
and editing format remain unchanged.

The default face inherits `shadow`; graphical text is 90% of normal size.
Terminals use subdued colours at the terminal's normal font size. Customize:

```elisp
(setq org-xob-tui-text-scale 0.9)
(setq org-xob-tui-panels
      '((backlinks north-west 32 9)
        (forlinks north-east 32 9)
        (preview south-east 54 14)))
;; M-x customize-face RET org-xob-tui-text
```

Hide and reopen panels after changing their initial styling or dimensions.
Visible context follows source edits after a short idle delay. Snapshots prefer
unsaved XOB edits and consult the existing Org ID index without rescanning it.
Use `C-c x g` after editing a linked node elsewhere.

This first renderer is intentionally limited: complex tables, description lists,
LaTeX, images, and export-specific formatting do not have full Org rendering
fidelity. The preview is a reading surface; use `C-c x RET` for editing.

## Disable and test

`M-x org-xob-tui-mode` disables the adapter, closes its child frames, and restores
the previous context renderer. The old split is not reopened automatically;
XOB's normal layout commands become available again.

For manual testing, try a node with both backlink and forward-link entries, type
while the panels are visible, pin a linked preview, toggle each panel, move and
resize it, and resize the parent frame. Check the appearance with your own theme
in GUI and terminal Emacs. This branch is intended for refinement before merging.

CI runs the existing tests plus snapshot, routing, preservation, and cleanup
regressions. On Emacs 31.1 it also drives a real terminal to verify three native
child frames, continued source input, actual toggle keys, pinning, movement,
resizing, and disposal without splitting or rewriting the editor.
