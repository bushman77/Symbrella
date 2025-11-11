// apps/symbrella_web/assets/js/hooks/chat.js
// Single-responsibility: ChatInput hook only.

// Helpers
const isPrintable = (e) =>
  e.key && e.key.length === 1 && !e.ctrlKey && !e.metaKey && !e.altKey

const isNavKey = (k) =>
  ["ArrowLeft","ArrowRight","ArrowUp","ArrowDown","Backspace","Delete","Tab"].includes(k)

export const ChatInput = {
  mounted() {
    const ta = this.el
    this.submitting = false
    this.composing  = false

    // ---- auto-resize
    this.resize = () => {
      ta.style.height = "auto"
      ta.style.height = Math.min(ta.scrollHeight, 240) + "px"
    }

    // ---- IME (composition) guards
    this.onCompStart = () => (this.composing = true)
    this.onCompEnd   = () => (this.composing = false)

    // ---- key handling with self-heal for printable char drop
    this.onKeyDown = (e) => {
      if (e.isComposing || this.composing) return

      // Printable: let it flow; if some other handler swallows it,
      // re-insert the char on the next frame.
      if (isPrintable(e)) {
        const before = ta.value
        const pos    = ta.selectionStart ?? before.length
        const ch     = e.key
        requestAnimationFrame(() => {
          // If nothing changed (key got swallowed), insert it ourselves.
          if (ta.value === before && document.activeElement === ta) {
            ta.setRangeText(ch, pos, pos, "end")
            ta.dispatchEvent(new Event("input", { bubbles: true }))
          }
        })
        return
      }

      // Navigation keys: let them pass
      if (isNavKey(e.key)) return

      // Enter to send (no Shift): push LiveView event, don't submit the form
      if (e.key === "Enter" && !e.shiftKey) {
        e.preventDefault()
        if (this.submitting) return
        this.submitting = true

        this.pushEvent("send", { message: ta.value })

        setTimeout(() => (this.submitting = false), 120)
        return
      }

      // Optional: ESC to blur
      if (e.key === "Escape") ta.blur()
    }

    ta.addEventListener("keydown", this.onKeyDown)
    ta.addEventListener("input", this.resize)
    ta.addEventListener("compositionstart", this.onCompStart)
    ta.addEventListener("compositionend", this.onCompEnd)

    ta.setAttribute("enterkeyhint", "send")
    this.resize()
  },

  updated() {
    this.resize?.()
  },

  destroyed() {
    const ta = this.el
    ta.removeEventListener("keydown", this.onKeyDown)
    ta.removeEventListener("input", this.resize)
    ta.removeEventListener("compositionstart", this.onCompStart)
    ta.removeEventListener("compositionend", this.onCompEnd)
  }
}

