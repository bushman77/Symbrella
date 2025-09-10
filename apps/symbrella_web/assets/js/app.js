import "phoenix_html"
import { Socket } from "phoenix"
import { LiveSocket } from "phoenix_live_view"
import topbar from "../vendor/topbar"

const Hooks = {}

/* -------------------- ScrollOnEvent (replaces AutoScroll) -------------------- */
/* Only scrolls when LiveView pushes "chat:scroll". Does one initial scroll
   on mount without animation; no auto-scroll on every DOM patch. */
// replaces Hooks.ScrollOnEvent
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
      // Hard fallback for mobile browser chrome shifts
      const doc = document.scrollingElement || document.documentElement
      window.scrollTo({ top: doc.scrollHeight, behavior: smooth ? "smooth" : "auto" })
      // Keep focus on the input without yanking the viewport further
      const input = document.getElementById("chat-input")
      if (input && !input.disabled) {
        setTimeout(() => input.focus({ preventScroll: true }), 50)
      }
    }

    // Initial settle: align messages bottom after first paint
    requestAnimationFrame(() => requestAnimationFrame(() => scrollMessagesBottom(false)))

    this.handleEvent("chat:scroll", (payload = {}) => {
      const to = payload.to || "messages"
      if (to === "composer") {
        // Make sure both the latest message and the composer are visible
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
    // Don’t yank the viewport when the textarea is disabled (e.g., during “Thinking…”)
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

/* -------------------- LiveSocket bootstrap -------------------- */
let csrfToken = document.querySelector("meta[name='csrf-token']")?.getAttribute("content")
let liveSocket = new LiveSocket("/live", Socket, { hooks: Hooks, params: { _csrf_token: csrfToken } })

topbar.config({ barThickness: 3, barColors: { 0: "#10b981" }, shadowColor: "rgba(0,0,0,0.3)" })
window.addEventListener("phx:page-loading-start", () => topbar.show())
window.addEventListener("phx:page-loading-stop", () => topbar.hide())

liveSocket.connect()
window.liveSocket = liveSocket

