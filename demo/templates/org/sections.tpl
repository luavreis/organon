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
    <section id="!(section:anchor)">
      <section:h-n><headline /></section:h-n>
      <section:children/>
      <section:subsections/>
    </section>
  </sections>
</case>

<case over-level>
  <ol>
    <sections>
      <li>
        <a id="!(section:anchor)"></a><headline /><br />
        <section:children/>
        <section:subsections/>
      </li>
    </sections>
  </ol>
</case>
