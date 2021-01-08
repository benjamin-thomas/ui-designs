# Implementation of various UI components in HTML/CSS

This is just me trying to reproduce various designs found on the web.

I try to keep the HTML markup as lean as possible (no extra
 divs for styling, etc.)

Keeping for reference and inspiration.

## Development

Use this tool

```bash
npm install -g live-server
live-server --no-browser --no-css-inject
```

Note to self: only use relative links, ok for dev env and live github
 pages.

Cleanup HTML with vim and this regex:

```vim
style=".\{-}"
```

It seems `\{-}` is vim's non greedy match version of `*?`

So basically use this:

```vim
# Remove attributes
:%s:class=".\{-}"::g
:%s:id=".\{-}"::g

# Reset hrefs
:%s:href=".\{-}":href="#":g

# Remove data tags
:%s:data-.\{-}".\{-}"::g
```

---

## Go to live environment

### Step 1

Navigate to [my github pages](https://benjamin-thomas.github.io/ui-designs/)

### Step 2

Visualise any of these pages

1. [Tailwind signup](001-tailwind-signup)
2. [Unishop table](002-unishop-table)
3. [Apland navbar (mobile navbar needs more work)](003-appland)
4. [Sidebar nav menu](004-sidebar-nav)
5. [Street Food](005-street-food)
