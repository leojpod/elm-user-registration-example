module.exports = {
  purge: ['src/elm/**/*.elm', 'src/typescript/**/*.ts'],
  theme: {
    extend: {}
  },
  variants: {
    borderColor: ['responsive', 'hover', 'focus', 'focus-within'],
    boxShadow: ['responsive', 'hover', 'focus', 'focus-within']
  },
  plugins: []
}
