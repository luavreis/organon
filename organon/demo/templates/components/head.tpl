<head>
  <meta charset="utf-8"/>

  <base href="/"/>

  <title_><doc:prop:title/></title_>

  <!-- Here, the use of "!(asset:path/to/asset.ext)"
       creates a link that is dynamically updated whenever
       the asset file is changed on disk. This ensures
       the browser properly refreshes the file when you
       make edits to it. -->
  <link rel="stylesheet" href="!(asset:css/tufte.css)"/>
  <link rel="stylesheet" href="!(asset:css/organon.css)"/>
  <link rel="stylesheet" href="!(asset:css/tables.css)"/>

  <components:katex />

  <meta name="viewport" content="width=device-width, initial-scale=1">
</head>
