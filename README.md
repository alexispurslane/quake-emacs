<div align="center">
  <img src="./banner-quake.png" height="128" style="display: block; margin: 0 auto"/>
  <h1>Quake Emacs</h1>
  <p>Lean, fast, focused, and based on the latest Emacs tech</p>
</div>

<p align="center">
<img src="./screenshots/dashboard.png" width="24%"/>
</p>

---

## Why?

If you want to use Emacs, but you:

1. just want to sit down and have your new editor work well and look good *right away*, and
2. don't want the complexity, abstraction, and lack of documentation of an Emacs distribution,

then Quake Emacs is for you. People tend to use Emacs for three main things: **code editing**, **writing**, and **note-taking**. Quake Emacs aims to provide a lean, fast, focused, and modern experience for all three of those purposes right out of the box, while remaining **one single 1000-line file** by leveraging vanilla Emacs's capabilities as much as possible.

## How to install

1. First, you install Quake Emacs like usual:

```bash
git clone https://github.com/alexispurslane/quake-emacs.git ~/.emacs.d
```

2. Then copy the example `user.el` provided with Quake to your Quake Emacs configuration directory at `~/.quake.d/user.el`:

```bash
mkdir -p ~/.quake.d/ && cp user.el ~/.quake.d/
```

To update, just `git pull` to the latest tag.

## Key Features

- 🎯 **Lean and focused**: Quake Emacs includes just the packages and configuration you need for a beautiful, modern IDE-lite experience when coding and a focused, ergonomic experience when writing or taking notes, and *nothing else*. Consider Quake Emacs part of your editor — it's just there to give you a good out of the box experience, like [Helix](https://helix-editor.com/). The rest is up to you! 
- 🚀 **Fast**: On my machine, Quake Emacs loads in 0.58 seconds on average. Every single package is carefully chosen with performance in mind, and the default load order is tuned obsessively to ensure Emacs starts as fast as possible. Enjoy fast startup times, or use it as extra headroom to add your own packages.
- 🥇 **Just one single file**: The configuration framework itself is only one simple, clear, self-contained 1000-line file. That's it. Just put the file in your emacs directory and go. No external commands, no multiple thousands of lines of Lisp scattered throughout hundreds of files across interminable layers of abstraction. If you want to know where something is, or how it works, it's easy to find.
- 🌐 **Vanilla-first**: Quake Emacs prioritizes using Emacs's built-in capabilities as much as possible, and when those fail, using packages that integrate well with that existing Emacs functionality and leverage it strategically. Quake Emacs only introduces layers of abstraction over built-in vanilla Emacs when *absolutely necessary*. Vanilla Emacs instructions should still work in Quake Emacs!
- 🖱 **GUI-maximalist**: One of Emacs's most unique features as an editor is its ability to combine the compactness, focus, and performance of a text-mode interface with embedded images, multiple fonts and font sizes, etc to give you a modern editor experience. Quake Emacs is designed to maximize its use of that right out of the box!

## Showcase

### Code Editing

Quake Emacs has fuzzy autocompletion with [Corfu](https://elpa.gnu.org/packages/doc/corfu.html) and Orderless enabled everywhere.

![](./screenshots/fast-autocompletion-everywhere1.png)

Full autocompletion even works in the Lisp Eval line (`M-:`). It's a real repl, so why not have a real coding experience in it?

![](./screenshots/fast-autocompletion-everywhere2.png)

Fuzzy searching with [Vertico](https://github.com/minad/vertico), [Marginalia](https://github.com/minad/marginalia), [Consult](https://github.com/minad/consult) and [Orderless](https://github.com/oantolin/orderless) is enabled in every selection menu and prompt. 

![](./screenshots/fuzzy-searching-everywhere.png)

Quake Emacs has an optionally pretty rich GUI with inclusion of [Centaur Tabs](https://github.com/ema2159/centaur-tabs) and [Treemacs](https://github.com/Alexander-Miller/treemacs) (only loaded when requested, so if you don't use them, you don't pay for them), both configured to look their best. (Also notice [hydra](https://github.com/abo-abo/hydra?tab=readme-ov-file), [which-key](https://github.com/abo-abo/hydra?tab=readme-ov-file), and my meticulous [evil-mode](https://github.com/emacs-evil/evil) leader key keybindings, made with [general](https://github.com/noctuid/general.el)).

![](./screenshots/optionally-ui-rich.png)

But Quake Emacs is also minimal and focused by default, if you prefer.

![](./screenshots/optionally-minimal.png)

While also offering IDE-class features besides completion, via [Eglot](https://github.com/joaotavora/eglot), [eldoc-box](https://github.com/casouri/eldoc-box), [tree-sitter](https://www.emacswiki.org/emacs/Tree-sitter), and [treesit-auto](https://github.com/renzmann/treesit-auto).

![](./screenshots/ide-class-features1.png)
![](./screenshots/ide-class-features2.png)
![](./screenshots/pretty-doc-tooltips.png)

And, Quake Emacs wouldn't be able to live up to its name unless it had a classic Quake-style popup terminal! So here it is, implemented entirely without any external packages, and bound to `SPC ~`:

![](./screenshots/quake-term.gif)

### Writing

For those of you who prefer to write your prose in Emacs, I've also created an excellent writing mode, which switches to [a variable pitch font](https://github.com/iaolo/iA-Fonts/tree/master) of your choice, enables a [distraction-free writing mode](https://github.com/joaotavora/darkroom), enables [visual fill column mode](https://github.com/joostkremers/visual-fill-column), wrapped at 65 characters, so that lines behave pleasingly like in a WYSIWYG editor, and enables a flymake [proselint](https://github.com/amperser/proselint) backend to help you improve your prose. To enable all of that, just use `SPC o d`! Here's a sneak peak:

![](./screenshots/proselint-enabled-writing-mode.png)

### Note-taking

For those that want Emacs to serve as their second brain, Quake Emacs uses [Denote](https://protesilaos.com/emacs/denote), a lightweight personal hypertext information manager that can incorporate an extensible list of markup languages (*not just org*), external files (links to and from, including buttonization as a minor-mode), and even non-text-markup files (such as PDFs, images, or code files like iPython Notebooks) directly into its linking and searching system. It offers all of the same basic features as `org-roam` without locking you down to using only Org, or requiring you to use an SQLite database, while also offering optional excellent integration with org if you want it. It also makes deep use of existing Emacs built-ins, as well as integrating explicitly with packages like vertico, marginalia, and consult via [consult-notes](https://github.com/mclear-tools/consult-notes). I've even created a set of convenient leader key keybinds for managing it.

![](./screenshots/denote.png)

## Justification

With the introduction of various modern Emacs features in the last few years, and the emergence of a new generation of Emacs packages focused on integrating with vanilla Emacs, Emacs distributions as we have known them are less and less relevant.

Now that `use-package` is included with Emacs by default, configuration frameworks are less necessary then ever, as vanilla Emacs's built-in capabilities are likely clearer, faster, and more powerful than whatever a configuration framework could offer, with the benefit of also being the community standard. In my experience, Doom Emacs's ideosyncratic package management system was less clear, less-organized, less-documented, and much less reliable than what is now built into Emacs 29.

Likewise, with the inclusion of `eglot` and `tree-sitter`, language-specific "layers" that compose five or six packages, together with a lot of configuration, in order to give a decent experience, are mostly a thing of the past. Quake Emacs leverages these powerful built-in modern Emacs features to give you the experience of an Emacs configuration framework and distribution, without all the downsides.

Similarly, with the creation of amazing packages like `vertico`, `orderless`, and `corfu`, the need to manually integrate added features from various packages into your Emacs system and other Emacs packages is basically obsolete: these packages integrate directly with Emacs, by hooking into or outright replacing Emacs's built in functions for performing various actions, so there's no need to do anything. As a result, much of the configuration work Emacs distributions needed to do to wire everything up simply doesn't need to be done.

---

Inspired by [DOOM Emacs](https://github.com/doomemacs/doomemacs) and [MinEmacs](https://github.com/abougouffa/minemacs/tree/main) (I owe the basic leader key configuration to MinEmacs).
