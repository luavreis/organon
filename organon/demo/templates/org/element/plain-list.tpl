<bind checkbox-def>
  <switch checkbox>
    <case true><code>[X]</code></case>
    <case false><code>[ ]</code></case>
    <case partial><code>[-]</code></case>
  </switch>
</bind>

<case unordered>
  <bind-text bullet-style>
    <switch bullet>
      <case tag="-">disc</case>
      <case tag="+">square</case>
      <case tag="*">circle</case>
    </switch>
  </bind-text>

  <ul affiliated style="list-style-type: !(bullet-style);">
    <list-items>
      <li>
        <checkbox-def />
        <list-item-content />
      </li>
    </list-items>
  </ul>
</case>

<case ordered-num>
  <ol affiliated style="list-style-type: decimal;">
    <list-items>
      <li value="!(counter-set)">
        <checkbox-def />
        <list-item-content />
      </li>
    </list-items>
  </ol>
</case>

<case ordered-alpha>
  <ol affiliated style="list-style-type: lower-alpha;">
    <list-items>
      <li value="!(counter-set)">
        <checkbox-def />
        <list-item-content />
      </li>
    </list-items>
  </ol>
</case>

<case descriptive>
  <dl affiliated>
    <list-items>
      <dt><checkbox-def /><descriptive-tag /></dt>
      <dd><list-item-content /></dd>
    </list-items>
  </dl>
</case>
