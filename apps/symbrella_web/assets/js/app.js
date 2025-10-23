// apps/symbrella_web/assets/js/app.js

import "phoenix_html"
import { Socket } from "phoenix"
import { LiveSocket } from "phoenix_live_view"
import topbar from "../vendor/topbar"

const Hooks = {}

/* -------------------- ScrollOnEvent (replaces AutoScroll) -------------------- */
/* Only scrolls when LiveView pushes "chat:scroll". Does one initial scroll
   on mount without animation; no auto-scroll on every DOM patch. */
Hooks.ScrollOnEvent = {
  mounted() {
    const scrollMessagesBottom = (smooth = true) => {
      const bottom = this.el.querySelector("#bottom")
      if (bottom) {
        bottom.scrollIntoView({ behavior: smooth ? "smooth" : "auto", block: "end" })
      } else {
        this.el.scrollTo({ top: this.el.scrollHeight, behavior: smooth ? "smooth" : "auto" })
      }
    }

    const scrollComposerIntoView = (smooth = true) => {
      const composer = document.getElementById("chat-composer")
      if (composer) {
        composer.scrollIntoView({ behavior: smooth ? "smooth" : "auto", block: "end" })
      }
      const doc = document.scrollingElement || document.documentElement
      window.scrollTo({ top: doc.scrollHeight, behavior: smooth ? "smooth" : "auto" })
      const input = document.getElementById("chat-input")
      if (input && !input.disabled) {
        setTimeout(() => input.focus({ preventScroll: true }), 50)
      }
    }

    requestAnimationFrame(() => requestAnimationFrame(() => scrollMessagesBottom(false)))

    this.handleEvent("chat:scroll", (payload = {}) => {
      const to = payload.to || "messages"
      if (to === "composer") {
        scrollMessagesBottom(true)
        scrollComposerIntoView(true)
      } else {
        scrollMessagesBottom(true)
      }
    })
  }
}

/* ----------- CSS size vars for header/footer ----------- */
function makeSizer(varName, fallbackPx) {
  return {
    mounted() {
      const root = document.documentElement
      this.apply = () => {
        const h = this.el?.getBoundingClientRect().height || fallbackPx
        root.style.setProperty(varName, `${h}px`)
      }
      this._ro = new ResizeObserver(this.apply)
      this._ro.observe(this.el)
      window.addEventListener("resize", this.apply)
      this.apply()
    },
    destroyed() {
      this._ro?.disconnect()
      window.removeEventListener("resize", this.apply)
    }
  }
}

Hooks.HeaderSizer   = makeSizer("--hdr", 56)
Hooks.FooterSizer   = makeSizer("--ftr", 72)
Hooks.ComposerSizer = makeSizer("--composer-h", 72)

/* -------------------- ChatInput -------------------- */
/* Auto-resize (cap 240px), Enter=send (Shift+Enter newline),
   IME-safe, de-duped submit, keeps focus after patches. */
Hooks.ChatInput = {
  mounted() {
    const ta = this.el
    this.submitting = false
    this.composing = false

    this.resize = () => {
      ta.style.height = "auto"
      ta.style.height = Math.min(ta.scrollHeight, 240) + "px"
    }

    this.onKeyDown = (e) => {
      if (e.key === "Enter" && !e.shiftKey && !this.composing) {
        e.preventDefault()
        if (this.submitting) return
        this.submitting = true
        const form = ta.closest("form")
        if (form) (form.requestSubmit ? form.requestSubmit() : form.submit())
        setTimeout(() => (this.submitting = false), 120)
      }
    }

    this.onCompStart = () => (this.composing = true)
    this.onCompEnd   = () => (this.composing = false)

    ta.addEventListener("input", this.resize)
    ta.addEventListener("keydown", this.onKeyDown)
    ta.addEventListener("compositionstart", this.onCompStart)
    ta.addEventListener("compositionend", this.onCompEnd)

    this.resize()
    ta.setAttribute("enterkeyhint", "send")
    ta.focus({ preventScroll: true })
  },
  updated() {
    this.resize?.()
    if (!this.el.disabled && document.activeElement !== this.el) {
      this.el.focus({ preventScroll: true })
    }
  },
  destroyed() {
    const ta = this.el
    ta.removeEventListener("input", this.resize)
    ta.removeEventListener("keydown", this.onKeyDown)
    ta.removeEventListener("compositionstart", this.onCompStart)
    ta.removeEventListener("compositionend", this.onCompEnd)
  }
}

/* -------------------- BrainMap (SVG click/keyboard interactivity) -------------------- */
/*
Usage:
  - In your HEEx where you render the SVG, wrap it with a container:
      <div id="brain-map" phx-hook="BrainMap"> ...inline SVG here... </div>
  - (Optional) mark elements with data-region="lifg|ofc|hippocampus|..." for precise mapping.
  - If data-region is absent, we fall back to inkscape:label or id from the nearest <g> ancestor.
Events pushed:
  - "select-region", payload: {region: "<key>"}
  - "hover-region",  payload: {region: "<key>", on: true|false}
*/
Hooks.BrainMap = {
  mounted() {
    this._handlers = []
    this.scan()
  },
  updated() {
    // SVG can be re-patched; re-scan and re-bind listeners
    this.unbindAll()
    this.scan()
  },
  destroyed() {
    this.unbindAll()
  },

  scan() {
    this.svg = this.el.querySelector("svg")
    if (!this.svg) return

    // Make the SVG keyboard-focusable
    this.svg.setAttribute("tabindex", "0")
    this.svg.setAttribute("role", "application")

    // Candidates:
    //  - anything with [data-region]
    //  - or group layers with inkscape:label / id (fallback)
    const candidates = Array.from(this.svg.querySelectorAll("[data-region], g[inkscape\\:label], g[id], path[id]"))

    // Deduplicate by the topmost <g> (prefer larger clickable areas)
    const unique = new Set()
    const pick = []
    for (const el of candidates) {
      const key = this.regionKey(el)
      if (!key) continue
      if (unique.has(el)) continue
      unique.add(el)
      pick.push({ el, key })
    }

    // Bind events
    pick.forEach(({ el, key }) => {
      // Visual affordances
      el.style.cursor = "pointer"
      el.setAttribute("tabindex", "0")
      el.setAttribute("role", "button")
      el.setAttribute("aria-label", `Select region ${key}`)

      const onClick = (e) => {
        e.preventDefault()
        this.highlight(el)
        this.pushEvent("select-region", { region: key })
      }
      const onKey = (e) => {
        if (e.key === "Enter" || e.key === " ") {
          e.preventDefault()
          this.highlight(el)
          this.pushEvent("select-region", { region: key })
        }
      }
      const onEnter = () => {
        el.dataset._hover = "1"
        this.pushEvent("hover-region", { region: key, on: true })
      }
      const onLeave = () => {
        delete el.dataset._hover
        this.pushEvent("hover-region", { region: key, on: false })
      }

      el.addEventListener("click", onClick)
      el.addEventListener("keydown", onKey)
      el.addEventListener("mouseenter", onEnter)
      el.addEventListener("mouseleave", onLeave)
      this._handlers.push([el, "click", onClick], [el, "keydown", onKey], [el, "mouseenter", onEnter], [el, "mouseleave", onLeave])
    })
  },

  unbindAll() {
    (this._handlers || []).forEach(([el, evt, fn]) => {
      try { el.removeEventListener(evt, fn) } catch(_) {}
    })
    this._handlers = []
  },

  regionKey(el) {
    // 1) explicit data-region takes precedence
    const explicit = el.getAttribute?.("data-region")
    if (explicit) return this.normalize(explicit)

    // 2) walk up to a <g> that has an inkscape:label or id
    let node = el
    while (node && node !== this.svg) {
      if (node.getAttribute) {
        const lab = node.getAttribute("inkscape:label") || node.getAttribute("label")
        if (lab) return this.normalize(lab)
        const id  = node.id
        if (id) return this.normalize(id)
      }
      node = node.parentNode
    }
    return null
  },

  normalize(s) {
    const k = String(s).trim().toLowerCase().replace(/\s+/g, "_").replace(/[^\w]/g, "_")
    // Map common labels from the provided SVG (e.g., "Cerebrellum" typo) to app keys
    const aliases = {
      cerebrellum: "cerebellum",
      cerebellum: "cerebellum",
      cortex: "frontal", // generic fallback
      eyes: "temporal",
      pons: "thalamus",
      lifg: "lifg",
      ofc: "ofc",
      hippocampus: "hippocampus",
      pmtg: "pmtg",
      temporal: "temporal",
      frontal: "frontal",
      thalamus: "thalamus"
    }
    return aliases[k] || k
  },

  highlight(el) {
    // Lightweight highlight: add a transient outline
    el.dataset._selected = "1"
    const prev = el.style.filter
    el.style.filter = "drop-shadow(0 0 6px rgba(16,185,129,0.85))"
    setTimeout(() => {
      if (!el.dataset._hover) {
        delete el.dataset._selected
        el.style.filter = prev || ""
      }
    }, 450)
  }
}

/* (Optional) A minimal live workspace feed hook if you want auto-trim/auto-scroll
   Attach with: <div id="workspace" phx-hook="WorkspaceFeed" data-autoscroll="1"></div> */
Hooks.WorkspaceFeed = {
  mounted() {
    this.max = parseInt(this.el.dataset.max || "200", 10)
    this.autoscroll = this.el.dataset.autoscroll === "1"
  },
  updated() {
    // Trim children if they grow too many
    if (this.max && this.el.children.length > this.max) {
      const excess = this.el.children.length - this.max
      for (let i = 0; i < excess; i++) this.el.removeChild(this.el.firstElementChild)
    }
    if (this.autoscroll) {
      this.el.scrollTop = this.el.scrollHeight
    }
  }
}

/* -------------------- LiveSocket bootstrap -------------------- */
let csrfToken = document.querySelector("meta[name='csrf-token']")?.getAttribute("content")
let liveSocket = new LiveSocket("/live", Socket, { hooks: Hooks, params: { _csrf_token: csrfToken } })

topbar.config({ barThickness: 3, barColors: { 0: "#10b981" }, shadowColor: "rgba(0,0,0,0.3)" })
window.addEventListener("phx:page-loading-start", () => topbar.show())
window.addEventListener("phx:page-loading-stop", () => topbar.hide())

liveSocket.connect()
window.liveSocket = liveSocket

