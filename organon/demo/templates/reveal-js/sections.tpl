<bind tag="headline">
  <span class="headline" style="display: flex; justify-content: space-between;">
    <span class="title">
      <section:headline/>
    </span>
    <span class="meta" style="text-align: right">
      <if priority>
        <span class="priority !(priority)">Priority: <priority/></span>
      </if>
      <if todo-name>
        <span class="todokw !(todo-state)">
          <todo-name/>
        </span>
      </if>
      <section:tags>
        <span class="tag tag-!(tag)"><tag/></span>
      </section:tags>
    </span>
  </span>
</bind>

<case normal>
  <sections>
    <if section:prop:noslide>
      <section:h-n><headline /></section:h-n>
      <section:children/>
      <section:subsections/>
      <else />
      <section id="!(section:anchor)">
        <section:h-n><headline /></section:h-n>
        <section:children/>
        <section:subsections/>
      </section>
    </if>
  </sections>
</case>

<case over-level>
  <ol>
    <sections>
      <li>
        <a id="!(anchor)"></a><headline /><br />
        <children/>
        <subsections/>
      </li>
    </sections>
  </ol>
</case>
