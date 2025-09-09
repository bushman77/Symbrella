import "phoenix_html"
import { Socket } from "phoenix"
import { LiveSocket } from "phoenix_live_view"
import topbar from "../vendor/topbar"

const Hooks = {}

/* -------------------- AutoScroll -------------------- */
Hooks.AutoScroll = {
  mounted() {
    this.scrollToBottom = (smooth = true) => {
      // prefer an anchor if present, else scroll container
      const bottom = this.el.querySelector("#bottom")
      if (bottom) {
        bottom.scrollIntoView({ behavior: smooth ? "smooth" : "auto", block: "end" })
      } else {
        this.el.scrollTop = this.el.scrollHeight
      }
    }
    this.scrollToBottom(false)
    this.handleEvent("chat:scroll", () => this.scrollToBottom())
  },
  updated() { this.scrollToBottom?.() }
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
    if (document.activeElement !== this.el) {
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

