# Implementation of various UI components in HTML/CSS.

This is just me trying to reproduce various designs found on the web.

I try to keep the HTML markup as lean as possible (no extra
 divs for styling, etc.)

Keeping for reference and inspiration.

## Development

Use this tool

```bash
npm install -g live-server
live-server --no-browser
```

Note to self: only use relative links, ok for dev env and live github
 pages.

Cleanup HTML with vim and this regex:

```
style=".\{-}"
```

It seems `\{-}` is vim's non greedy match version of `*?`

So basically use this:

```
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

1. [tailwind-signup.html](tailwind-signup.html)
2. [unishop-table.html](unishop-table.html)
2. [Apland navbar (mobile navbar needs more work)](apland-navbar.html)



