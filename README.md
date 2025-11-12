# org-hover

An Emacs package that provides hover preview functionality for org links.

![org-hover](org-hover.png)

## Features

- **Link Preview**: Preview content of file links in org-mode
- **Block Preview**: Preview content of `#+INCLUDE:` blocks
- **Smart Positioning**: Intelligently position popup windows based on screen boundaries
- **Adaptive Sizing**: Automatically adjust preview window size based on content
- **Auto-hide**: Close popup by clicking outside or configure auto-hide after delay

## Installation

### Manual Installation

```bash
git clone https://github.com/VandeeFeng/org-hover.git
```

Then add to your `init.el`:

```elisp
(add-to-list 'load-path "/path/to/org-hover")
(require 'org-hover)
```

## Usage

Basic Usage:

1. In an org file, place cursor on an org file link
2. Press `C-c h l` to preview the link
3. Press `C-c h f` to preview any specified file
4. Press `C-c h b` to preview the `#+begin_quote:` block at point
5. Press `C-c h B` to preview the `#+begin_quote:` block at point ,and insert the content

example:

```elisp
#+begin_quote
#+INCLUDE: "file.org" :lines "1-30"
#+end_quote
```

Press `C-c h b` ,get a hover preview of the 1-30 line of the file.

## Configuration Options

`org-hover-auto-hide` :

Whether to automatically hide the popup window. Disabled by default.

```elisp
(setq org-hover-auto-hide t)  ; Default: nil (disabled)
```

`org-hover-auto-hide-delay` :

Delay in seconds before auto-hiding the popup window. Only effective when `org-hover-auto-hide` is enabled.

```elisp
(setq org-hover-auto-hide-delay 5)  ; Default: 5 seconds
```

`org-block-hover-auto-insert` :

Whether to automatically insert the content to the quote block

```elisp
(setq org-block-hover-auto-insert t) ; Default: nil
```
