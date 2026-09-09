// Custom Shiny.InputBinding backing treeSelectInput() (see app.R). Renders
// nested tree_data (Industry's Aggregate -> 2-digit hierarchy) as a
// collapsible tree, or flat tree_data (Variable -- every node a root, no
// children) as a plain scrollable list -- same markup and
// binding either way, since a childless row just renders with its expand
// arrow hidden (see .tree-select-row.no-children in app.R's CSS). The
// toggle itself is a text <input>, not a button -- clicking (or tabbing)
// into it opens the panel *and* blanks the box (rather than leaving the
// current selection there to be typed over), the same "click in, get a
// blank search/scroll list, picking nothing puts the old value back" feel
// as Shiny's own selectize single-select pickers, just without a second
// search box revealed below it.
//
// R hands us a minimal skeleton -- a div.tree-select (the bound element)
// holding a data-selected/data-placeholder pair and an embedded
// <script type="application/json"> with the tree_data. Everything visible
// (toggle input, chevron, panel, rows) is built here in
// initialize()/renderTree(), and receiveMessage() re-runs the same
// renderTree() so there's exactly one place that builds this DOM instead
// of a server-rendered version that could drift from a JS-rebuilt one.
(function () {
  "use strict";

  function escapeHtml(str) {
    return String(str)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;")
      .replace(/'/g, "&#39;");
  }

  // Plain-object stand-ins for Map/Set (and helpers for the DOM/NodeList
  // methods used below) -- this widget is the *only* thing under each of
  // "Variable"/"Industry" (treeSelectInput() emits no server-rendered
  // fallback markup, just an empty div + the JSON payload), so if
  // this file throws anywhere during initialize() the whole control
  // silently stays blank with nothing else on the page indicating why.
  // Map/Set/NodeList.prototype.forEach/
  // Element.prototype.closest/the `:scope` combinator are all absent from
  // older engines that some collaborators' locked-down/managed machines may
  // still be on -- sticking to plain objects, arrays and manual loops here
  // costs nothing on modern browsers and removes that whole failure mode.
  function makeSet() { return Object.create(null); }
  function setAdd(set, v) { set[v] = true; }
  function setDelete(set, v) { delete set[v]; }
  function setHas(set, v) { return !!set[v]; }
  function cloneSet(set) {
    var out = Object.create(null);
    for (var k in set) if (Object.prototype.hasOwnProperty.call(set, k)) out[k] = true;
    return out;
  }
  function forEachOwn(obj, fn) {
    for (var k in obj) if (Object.prototype.hasOwnProperty.call(obj, k)) fn(obj[k], k);
  }
  function forEachNode(nodeList, fn) {
    for (var i = 0; i < nodeList.length; i++) fn(nodeList[i]);
  }
  // Walks up from `el` to the nearest ancestor (or itself) carrying
  // .tree-select-row -- a hand-rolled Element.prototype.closest() for the
  // one selector this file ever needs it for.
  function closestRow(el) {
    while (el && el.nodeType === 1) {
      if (el.classList && el.classList.contains("tree-select-row")) return el;
      el = el.parentNode;
    }
    return null;
  }

  function parseNodes(el) {
    var scriptEl = el.querySelector("script.tree-select-data");
    if (!scriptEl) return [];
    try {
      return JSON.parse(scriptEl.textContent || "[]");
    } catch (e) {
      return [];
    }
  }

  // One DFS over the nested tree building value -> {label, parentValue}.
  // Depth/hasChildren live on the DOM (elementMap) instead, since the DOM
  // is what search/expand actually manipulate.
  function buildIndex(nodes, parentValue, index) {
    nodes.forEach(function (node) {
      index[node.value] = { label: node.label, parentValue: parentValue };
      if (node.children && node.children.length) {
        buildIndex(node.children, node.value, index);
      }
    });
    return index;
  }

  function nodeHtml(node, depth) {
    var hasChildren = !!(node.children && node.children.length);
    var rowClass = "tree-select-row" + (hasChildren ? "" : " no-children");
    var html =
      '<li class="tree-select-node" data-value="' + escapeHtml(node.value) + '">' +
      '<div class="' + rowClass + '" data-value="' + escapeHtml(node.value) + '" ' +
      'style="padding-left:' + (depth * 1.1) + 'rem" aria-expanded="false">' +
      '<span class="tree-select-arrow" aria-hidden="true">&#9656;</span>' +
      '<span class="tree-select-label" role="treeitem" tabindex="0">' + escapeHtml(node.label) + "</span>" +
      "</div>";
    if (hasChildren) {
      html +=
        '<ul class="tree-select-children" role="group" hidden>' +
        node.children.map(function (child) { return nodeHtml(child, depth + 1); }).join("") +
        "</ul>";
    }
    html += "</li>";
    return html;
  }

  function getState(el) {
    return $(el).data("treeState");
  }

  // Last-resort path for the failure mode the comment above makeSet()
  // warns about: renderTree() throws for some reason this file didn't
  // anticipate (a still-missing DOM/engine feature, a malformed tree_data
  // payload, whatever), and initialize()/receiveMessage() catch it before
  // it can leave `el` permanently empty. Not as nice as the real tree +
  // search widget, but it's a real, working single-select picker built
  // from nothing but createElement/appendChild, so it doesn't lean on
  // any of the same DOM/engine features that might have just failed.
  function flattenNodes(nodes, depth, out) {
    nodes.forEach(function (node) {
      out.push({ value: node.value, label: node.label, depth: depth });
      if (node.children && node.children.length) {
        flattenNodes(node.children, depth + 1, out);
      }
    });
    return out;
  }

  function renderFallback(el, nodes, initialValue) {
    $(el).find(".tree-select-toggle, .tree-select-chevron, .tree-select-panel, .tree-select-fallback").remove();

    var flat = [];
    try {
      flat = flattenNodes(nodes || [], 0, []);
    } catch (e) {
      flat = []; // still renders a usable (if empty) picker -- see below
    }

    var select = document.createElement("select");
    select.className = "form-control tree-select-fallback";

    var placeholder = document.createElement("option");
    placeholder.value = "";
    placeholder.textContent = el.getAttribute("data-placeholder") || "Select an option...";
    select.appendChild(placeholder);

    flat.forEach(function (item) {
      var opt = document.createElement("option");
      opt.value = item.value;
      var indent = "";
      for (var i = 0; i < item.depth; i++) indent += "— "; // em dash + hair space: cheap depth cue, no CSS needed
      opt.textContent = indent + item.label;
      select.appendChild(opt);
    });

    select.value = initialValue || "";
    el.appendChild(select);

    $(el).data("treeState", { fallback: true, select: select, value: select.value });
    el.setAttribute("data-selected", select.value);
  }

  // Shared by treeSelectBinding.setValue() and receiveMessage()'s
  // "selected"-only branch -- routes to the real tree's setValue() or,
  // in fallback mode, just updates the <select> directly (setValue()
  // itself assumes state.toggleLabel/elementMap etc., which don't exist
  // once renderFallback() has run).
  function setValueAny(el, value) {
    var state = getState(el);
    if (state && state.fallback) {
      state.select.value = value || "";
      state.value = state.select.value;
      el.setAttribute("data-selected", state.value);
    } else {
      setValue(el, value);
    }
  }

  // (Re)builds the toggle button + dropdown panel from `nodes`, replacing
  // whatever was there before (used both at init and on receiveMessage()
  // with a new tree_data). The embedded <script> tag from R is left alone
  // -- it's only ever read once per renderTree() call, not re-derived from.
  function renderTree(el, nodes, initialValue) {
    $(el).find(".tree-select-toggle, .tree-select-chevron, .tree-select-panel").remove();

    // True when nothing in this tree_data has children at all (Variable's
    // flat list) rather than some rows being leaves alongside
    // genuine branches (Industry). CSS keys off this (.tree-select-flat)
    // to skip reserving every row's expand-arrow gutter -- that gutter is
    // only worth keeping (as blank space, via visibility:hidden below)
    // when at least one sibling row actually has a visible arrow for a
    // leaf's own label to line up against; a flat list has no such row,
    // so the reserved space just read as a stray indent.
    var isFlat = !nodes.some(function (node) { return node.children && node.children.length; });
    el.classList.toggle("tree-select-flat", isFlat);

    var index = buildIndex(nodes, null, {});
    var listHtml = nodes.map(function (node) { return nodeHtml(node, 0); }).join("");
    var placeholderText = el.getAttribute("data-placeholder") || "";
    var html =
      '<input type="text" class="tree-select-toggle" autocomplete="off" spellcheck="false" ' +
      'aria-haspopup="true" aria-expanded="false" placeholder="' + escapeHtml(placeholderText) + '">' +
      '<span class="tree-select-chevron" aria-hidden="true">&#9656;</span>' +
      '<div class="tree-select-panel" hidden>' +
      '<ul class="tree-select-list" role="tree">' + listHtml + "</ul>" +
      "</div>";
    $(el).append(html);

    // Plain object, not Map -- see the comment above makeSet(). Keyed by
    // node value; li.querySelector(".tree-select-row") (no `:scope >`)
    // still reliably returns *this* li's own direct row/children-list, not
    // some deeper-nested descendant's -- the direct child is always first
    // in document order, so querySelector's depth-first search finds it
    // before ever descending into it.
    var elementMap = Object.create(null);
    forEachNode(el.querySelectorAll(".tree-select-node"), function (li) {
      elementMap[li.getAttribute("data-value")] = {
        li: li,
        row: li.querySelector(".tree-select-row"),
        childList: li.querySelector(".tree-select-children")
      };
    });

    var toggle = el.querySelector(".tree-select-toggle");
    toggle.setAttribute("aria-controls", el.id ? el.id + "-panel" : "");

    var state = {
      index: index,
      elementMap: elementMap,
      expanded: makeSet(),
      expandSnapshot: null,
      searchActive: false,
      value: "",
      toggle: toggle,
      panel: el.querySelector(".tree-select-panel"),
      placeholder: el.getAttribute("data-placeholder") || ""
    };
    $(el).data("treeState", state);

    setValue(el, initialValue);
  }

  // Sets the selected value and, since the toggle input doubles as the
  // display for it, writes the matching label straight into the input's
  // value -- there's no separate "committed label" element to keep in
  // sync the way there was when the toggle was a plain button. The empty
  // ("" / no match) case is left to the input's native `placeholder`
  // attribute rather than a hand-styled placeholder span.
  function setValue(el, value) {
    var state = getState(el);
    if (!state) return;
    value = value || "";
    state.value = value;
    el.setAttribute("data-selected", value);

    var info = state.index[value];
    state.toggle.value = info ? info.label : "";

    forEachOwn(state.elementMap, function (refs, nodeValue) {
      refs.row.classList.toggle("selected", nodeValue === value);
    });
  }

  function setExpanded(el, value, expand) {
    var state = getState(el);
    var refs = state.elementMap[value];
    if (!refs || !refs.childList) return;
    if (expand) setAdd(state.expanded, value); else setDelete(state.expanded, value);
    refs.childList.hidden = !expand;
    refs.row.setAttribute("aria-expanded", expand ? "true" : "false");
    refs.row.classList.toggle("expanded", expand);
  }

  function expandAncestors(el, value) {
    var state = getState(el);
    var info = state.index[value];
    var parent = info ? info.parentValue : null;
    while (parent) {
      setExpanded(el, parent, true);
      var parentInfo = state.index[parent];
      parent = parentInfo ? parentInfo.parentValue : null;
    }
  }

  function openPanel(el) {
    var state = getState(el);
    if (!state.panel.hidden) return;
    state.panel.hidden = false;
    state.toggle.setAttribute("aria-expanded", "true");
    if (state.value) expandAncestors(el, state.value);
  }

  // Closes the panel and, since the toggle input was doubling as the
  // search box while it was open, restores the input's text back to the
  // committed selection's label (clearing whatever was typed) and clears
  // any active filter/expand-state so reopening starts from the real
  // selection again instead of a stale search. Safe to call whether or
  // not anything was actually typed -- applySearch(el, "") and
  // re-assigning the same label are both no-ops in that case.
  function closePanel(el) {
    var state = getState(el);
    if (!state || state.panel.hidden) return;
    state.panel.hidden = true;
    state.toggle.setAttribute("aria-expanded", "false");
    applySearch(el, "");
    var info = state.index[state.value];
    state.toggle.value = info ? info.label : "";
  }

  // Search filters to label-substring matches plus every ancestor of a
  // match (so the path down to it stays visible), hiding everything else.
  // On the first keystroke of a search "session" the current expand state
  // is snapshotted so clearing the box restores exactly that instead of
  // always collapsing back to root-only.
  function applySearch(el, rawQuery) {
    var state = getState(el);
    var query = rawQuery.trim().toLowerCase();

    if (query === "") {
      if (state.searchActive) {
        state.expanded = state.expandSnapshot || makeSet();
        state.expandSnapshot = null;
        state.searchActive = false;
        forEachOwn(state.elementMap, function (refs, value) {
          refs.li.hidden = false;
          if (refs.childList) {
            var expand = setHas(state.expanded, value);
            refs.childList.hidden = !expand;
            refs.row.setAttribute("aria-expanded", expand ? "true" : "false");
            refs.row.classList.toggle("expanded", expand);
          }
        });
      }
      return;
    }

    if (!state.searchActive) {
      state.expandSnapshot = cloneSet(state.expanded);
      state.searchActive = true;
    }

    var visible = makeSet();
    Object.keys(state.index).forEach(function (value) {
      var info = state.index[value];
      if (info.label.toLowerCase().indexOf(query) === -1) return;
      setAdd(visible, value);
      var parent = info.parentValue;
      while (parent) {
        setAdd(visible, parent);
        var parentInfo = state.index[parent];
        parent = parentInfo ? parentInfo.parentValue : null;
      }
    });

    forEachOwn(state.elementMap, function (refs, value) {
      var show = setHas(visible, value);
      refs.li.hidden = !show;
      if (refs.childList) {
        refs.childList.hidden = !show;
        refs.row.setAttribute("aria-expanded", show ? "true" : "false");
      }
    });
  }

  var treeSelectBinding = new Shiny.InputBinding();

  $.extend(treeSelectBinding, {
    find: function (scope) {
      return $(scope).find(".tree-select");
    },

    initialize: function (el) {
      var nodes = parseNodes(el);
      var initialValue = el.getAttribute("data-selected") || "";
      try {
        renderTree(el, nodes, initialValue);
      } catch (e) {
        if (window.console && console.error) {
          console.error(
            "treeSelectInput (#" + (el.id || "?") + "): failed to build the tree widget, " +
            "falling back to a plain <select>. Please report this error:", e
          );
        }
        renderFallback(el, nodes, initialValue);
      }
    },

    getValue: function (el) {
      var state = getState(el);
      if (!state) return "";
      return (state.fallback ? state.select.value : state.value) || "";
    },

    setValue: function (el, value) {
      setValueAny(el, value);
    },

    subscribe: function (el, callback) {
      // Clicking or tabbing into the toggle input is what opens the panel
      // -- and blanks the input (cursor at position 0, nothing selected)
      // rather than leaving/highlighting the current label, matching
      // selectize's own single-select behaviour (Variable/Industry):
      // its bundled CSS hides the selected .item while the dropdown is
      // open, showing just the empty search box underneath. closePanel()
      // is what puts the label back if nothing new gets picked.
      $(el).on("focus.treeSelect", ".tree-select-toggle", function (e) {
        openPanel(el);
        e.target.value = "";
      });

      // Row clicks/arrow-toggle clicks happen *inside* the still-focused
      // toggle input's panel -- without this, the mousedown would blur the
      // input (moving focus to the clicked row) before the click handlers
      // below ever run, closing/rebuilding the panel out from under the
      // click. Preventing mousedown's default focus-shift keeps the input
      // focused throughout, so those click handlers fire against a stable
      // panel exactly like the old separate-search-box version did.
      $(el).on("mousedown.treeSelect", ".tree-select-panel", function (e) {
        e.preventDefault();
      });

      $(el).on("click.treeSelect", ".tree-select-arrow", function (e) {
        e.stopPropagation();
        var row = closestRow(e.currentTarget);
        var value = row.getAttribute("data-value");
        var state = getState(el);
        setExpanded(el, value, !setHas(state.expanded, value));
      });

      function selectRow(row) {
        var value = row.getAttribute("data-value");
        setValue(el, value);
        closePanel(el);
        // Picking an option is a completed action -- unlike backing out of
        // an in-progress search (Escape/click-outside/tab-away, which all
        // leave the cursor exactly where it was), this should visibly
        // "click you out" rather than leave the cursor sitting in the
        // toggle. Blurs whichever of the two actually has focus here: the
        // toggle input for a mouse pick (kept focused throughout by the
        // mousedown-preventDefault above), or the row's own label span for
        // a keyboard pick (Enter/Space while tabbed onto it).
        if (document.activeElement && el.contains(document.activeElement)) {
          document.activeElement.blur();
        }
        callback(true);
      }

      $(el).on("click.treeSelect", ".tree-select-label", function (e) {
        selectRow(closestRow(e.currentTarget));
      });

      $(el).on("keydown.treeSelect", ".tree-select-label", function (e) {
        if (e.key === "Enter" || e.key === " ") {
          e.preventDefault();
          selectRow(closestRow(e.currentTarget));
        }
      });

      $(el).on("input.treeSelect", ".tree-select-toggle", function (e) {
        applySearch(el, e.target.value);
      });

      // Fires on any real focus loss -- Tab away, the click-outside
      // handler below, or selectRow()'s own deliberate blur() after a
      // pick. closePanel() is idempotent (a no-op once already closed),
      // so it's fine that a pick's blur arrives after selectRow() already
      // closed the panel itself.
      $(el).on("blur.treeSelect", ".tree-select-toggle", function () {
        closePanel(el);
      });

      $(el).on("keydown.treeSelect", function (e) {
        if (e.key === "Escape") {
          closePanel(el);
          getState(el).toggle.focus();
        }
      });

      // Bare (non-delegated) change handler -- this is what makes
      // $(el).trigger("change") in receiveMessage() actually notify Shiny,
      // the same idiom every built-in input binding uses.
      $(el).on("change.treeSelect", function () {
        callback(true);
      });

      $(document).on("click.treeSelect-" + el.id, function (e) {
        if (!el.contains(e.target)) closePanel(el);
      });
    },

    unsubscribe: function (el) {
      $(el).off(".treeSelect");
      $(document).off(".treeSelect-" + el.id);
    },

    receiveMessage: function (el, data) {
      var state = getState(el);
      var priorValue = state ? (state.fallback ? state.select.value : state.value) : (el.getAttribute("data-selected") || "");
      if (data.hasOwnProperty("tree_data")) {
        var nextValue = data.hasOwnProperty("selected") ? data.selected : priorValue;
        try {
          renderTree(el, data.tree_data, nextValue);
        } catch (e) {
          if (window.console && console.error) {
            console.error(
              "treeSelectInput (#" + (el.id || "?") + "): failed to re-render the tree widget, " +
              "falling back to a plain <select>. Please report this error:", e
            );
          }
          renderFallback(el, data.tree_data, nextValue);
        }
      } else if (data.hasOwnProperty("selected")) {
        setValueAny(el, data.selected);
      }
      $(el).trigger("change");
    }
  });

  Shiny.inputBindings.register(treeSelectBinding, "productivitydashboard.treeSelect");
})();
