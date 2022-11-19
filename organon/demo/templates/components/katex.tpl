<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.16.0/dist/katex.min.css" integrity="sha384-Xi8rHCmBmhbuyyhbI88391ZKP2dmfnOl4rT9ZfRI7mLTdk1wblIUnrIq35nqwEvC" crossorigin="anonymous">
<script defer src="https://cdn.jsdelivr.net/npm/katex@0.16.0/dist/katex.min.js" integrity="sha384-X/XCfMm41VSsqRNQgDerQczD69XqmjOOOwYQvr/uuC+j4OPoNhVgjdGFwhvN02Ja" crossorigin="anonymous"></script>
<script defer src="https://cdn.jsdelivr.net/npm/katex@0.16.0/dist/contrib/auto-render.min.js" integrity="sha384-+XBljXPPiv+OzfbB3cVmLHf4hdUFHlWNZN5spNQ7rmHTXpd7WvJum6fIACpNNfIR" crossorigin="anonymous"></script>
<script>
 var katexOptions = {
   delimiters: [
     {left: '$$', right: '$$', display: true},
     {left: '$', right: '$', display: false},
     {left: '\\(', right: '\\)', display: false},
     {left: '\\[', right: '\\]', display: true}
   ],
   macros: {
     "\\NN": "\\mathbb{N}",
     "\\ZZ": "\\mathbb{Z}",
     "\\QQ": "\\mathbb{Q}",
     "\\RR": "\\mathbb{R}",
     "\\CC": "\\mathbb{C}",
     "\\interval": context => {
       let arg = context.consumeArg(["]"]);
       let argtext = arg.tokens
                        .reduce((p, c) => c.text + p, "")
                        .slice(1);
       let a = "["
       let b = "]"
       switch (argtext) {
         case 'open':
           a = "("
           b = ")"
           break;
         case 'open left':
           a = "("
           break;
         case 'open right':
           b = ")"
           break;
       }
       return a + "#1,#2" + b;
     }
   },
   globalGroup: true,
   throwOnError: false
 };

 function renderMath() {
   let preamble = document.querySelector('meta[name="katex-preamble"]');
   if (preamble != null) {
     katex.renderToString(preamble.content, katexOptions);
   }
   let mathElements = document.body.querySelectorAll('span.math, div.math');
   for (let element of mathElements) {
     let display = element.classList.contains('display');
     katex.render(element.textContent, element, {
       ...katexOptions,
       displayMode: display,
     });
   }
 }

 document.addEventListener("DOMContentLoaded", () => {
   renderMath();

   window.addEventListener("EMAHotReload", () => {
     renderMath();
   })
 })
</script>
<if page:prop:katex_preamble>
  <meta name="katex-preamble" content="!(page:prop:katex_preamble)" />
</if>
