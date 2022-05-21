<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.13.11/dist/katex.min.css" integrity="sha384-Um5gpz1odJg5Z4HAmzPtgZKdTBHZdw8S29IecapCSB31ligYPhHQZMIlWLYQGVoc" crossorigin="anonymous">
<script defer src="https://cdn.jsdelivr.net/npm/katex@0.13.11/dist/katex.min.js" integrity="sha384-YNHdsYkH6gMx9y3mRkmcJ2mFUjTd0qNQQvY9VYZgQd7DcN7env35GzlmFaZ23JGp" crossorigin="anonymous"></script>
<script>
 katexOptions = {
   delimiters: [
     {left: '$$', right: '$$', display: true},
     {left: '$', right: '$', display: false},
     {left: '\\(', right: '\\)', display: false},
     {left: '\\[', right: '\\]', display: true}
   ],
   macros: {
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
 }

 function renderKatex() {
   renderMathInElement(document.body, katexOptions)
 }
</script>
<script defer src="https://cdn.jsdelivr.net/npm/katex@0.13.16/dist/contrib/auto-render.min.js"
        integrity="sha384-vZTG03m+2yp6N6BNi5iM4rW4oIwk5DfcNdFfxkk9ZWpDriOkXX8voJBFrAO7MpVl"
        crossorigin="anonymous"
        onload="renderKatex();"></script>
<style type="text/css" media="screen">
  .katex { font-size: 0.95em; }
</style>
