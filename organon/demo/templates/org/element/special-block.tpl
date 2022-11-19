<bind theorem>
  <div class="theorem" affiliated>
    <span class="theorem-name"><apply-content /></span>
    <if akw:caption><span style="font-style: normal"> (<akw:caption />)</span></if>.
    <content />
  </div>
</bind>

<bind note>
  <div class="note" affiliated>
    <div class="heading"><apply-content /></div>
    <div class="content"><content /></div>
  </div>
</bind>

<switch special-name>
  <case proof>
    <details open class="proof foldable">
      <summary class="title">
        <proof-name />.
      </summary>
      <content />
    </details>
  </case>

  <case solution>
    <details class="proof foldable">
      <summary class="title">
        Solution.
      </summary>
      <content />
    </details>
  </case>

  <case theorem>
    <theorem>Theorem</theorem>
  </case>

  <case lemma>
    <theorem>Lemma</theorem>
  </case>

  <case definition>
    <theorem>Definition</theorem>
  </case>

  <case proposition>
    <theorem>Proposition</theorem>
  </case>

  <case portal>
    <target tag="!(akw:portal-target)">
      <div id=!(akw:portal-target)>
        <content />
      </div>
    </target >
  </case>

  <default>
    <div class="!(special-name)" affiliated>
      <content />
    </div>
  </default>
</switch>
