/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    "../lib/**/*.*ex",   // HEEx/EX templates & components
    "./js/**/*.js",      // hooks
    "./css/**/*.css"     // any local CSS partials
  ],
  theme: { extend: {} },
  plugins: [
    // If those vendor files export Tailwind plugins, keep them here:
    // require("../vendor/heroicons"),
    // require("../vendor/daisyui"),
    // require("../vendor/daisyui-theme"),
  ]
}

