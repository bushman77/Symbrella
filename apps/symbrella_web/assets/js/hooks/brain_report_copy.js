// One-click export of all live Brain dashboard diagnostics.
//
// Read only the DOM that Phoenix has already rendered. This avoids an extra
// network request (which can lose clipboard user activation) and avoids
// serializing a second, potentially very large blackboard payload every 500 ms.

const reportText = (section, root) => {
  const selectors = (section.dataset.brainReportSource || "")
    .split(",")
    .map((selector) => selector.trim())
    .filter(Boolean)

  for (const selector of selectors) {
    const source = root.querySelector(selector)
    if (source) {
      const value = source.value ?? source.textContent ?? ""
      return value.trim() || "(no data)"
    }
  }

  // innerText preserves line breaks and excludes visually hidden copy targets.
  return (section.innerText || section.textContent || "").trim() || "(no data)"
}

export function buildBrainReport(root, capturedAt = new Date()) {
  if (!root) return ""

  const select = root.querySelector('select[name="region"]')
  const region = select?.value || "unknown"
  const sections = Array.from(root.querySelectorAll("[data-brain-report-section]"))

  const blocks = [
    "Symbrella — Complete Brain Diagnostics",
    "Captured: " + capturedAt.toISOString(),
    "Selected region: " + region,
    "Sections: " + sections.length
  ]

  for (const section of sections) {
    const heading = section.dataset.brainReportSection || "Diagnostics"
    blocks.push("\n===== " + heading + " =====\n" + reportText(section, root))
  }

  return blocks.join("\n").trim() + "\n"
}

async function writeClipboard(text) {
  if (navigator.clipboard?.writeText) {
    try {
      await navigator.clipboard.writeText(text)
      return true
    } catch (_) {
      // Fall back to selection copying, including on older Android browsers.
    }
  }

  const field = document.createElement("textarea")
  field.value = text
  field.readOnly = true
  field.setAttribute("aria-label", "Brain report clipboard fallback")
  field.style.cssText =
    "position:fixed;left:-9999px;top:0;width:1px;height:1px;opacity:0;"
  document.body.appendChild(field)

  let copied = false
  try {
    field.focus()
    field.select()
    field.setSelectionRange(0, field.value.length)
    copied = document.execCommand("copy") === true
  } catch (_) {
    copied = false
  } finally {
    field.remove()
  }

  return copied
}

function manualCopyDialog(report) {
  document.getElementById("brain-report-manual-copy")?.remove()

  const panel = document.createElement("div")
  panel.id = "brain-report-manual-copy"
  panel.setAttribute("role", "dialog")
  panel.setAttribute("aria-label", "Manually copy Brain report")
  panel.style.cssText =
    "position:fixed;z-index:10000;inset:12% 4% auto;background:#fff;color:#111;" +
    "border:2px solid #555;border-radius:10px;padding:12px;" +
    "box-shadow:0 8px 32px #0005;max-height:75vh;overflow:auto;"

  const msg = document.createElement("p")
  msg.textContent =
    "Browser clipboard access was blocked. The complete report is selected below; long-press and choose Copy."
  msg.style.marginBottom = "8px"

  const field = document.createElement("textarea")
  field.value = report
  field.readOnly = true
  field.setAttribute("aria-label", "Complete Brain report")
  field.style.cssText = "width:100%;height:42vh;font:12px monospace;white-space:pre;"

  const close = document.createElement("button")
  close.type = "button"
  close.textContent = "Close"
  close.style.cssText =
    "margin-top:8px;padding:6px 12px;border:1px solid #888;border-radius:6px;"
  close.addEventListener("click", () => panel.remove())

  panel.append(msg, field, close)
  document.body.appendChild(panel)
  field.focus()
  field.select()
}

export const BrainReportCopy = {
  mounted() {
    this.copyReport = async () => {
      const root = document.getElementById("brain-report-root")
      const report = buildBrainReport(root)
      const label = this.el.querySelector("[data-brain-copy-label]")
      if (!root || !report || !label) return

      if (this._busy) return
      this._busy = true
      const original = "Copy Full Brain Report"

      try {
        const copied = await writeClipboard(report)
        label.textContent = copied ? "Copied full report" : "Clipboard blocked"
        if (!copied) manualCopyDialog(report)
      } catch (_) {
        label.textContent = "Clipboard blocked"
        manualCopyDialog(report)
      } finally {
        this._busy = false
        clearTimeout(this._reset)
        this._reset = setTimeout(() => {
          if (this.el?.isConnected) label.textContent = original
        }, 2400)
      }
    }

    this.el.addEventListener("click", this.copyReport)
  },

  destroyed() {
    this.el.removeEventListener("click", this.copyReport)
    clearTimeout(this._reset)
  }
}
