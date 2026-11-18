// Linha do tempo da história econômica de Pernambuco
// Lê o JSON embutido em #dados-linha (gerado de config/linha_do_tempo.csv) e
// desenha uma faixa por atividade, com fases coloridas e marcos clicáveis, e
// embaixo os fatores transversais. Sem bibliotecas externas.
(function () {
  const NS = "http://www.w3.org/2000/svg";
  const FASES = {
    surgimento: { cor: "#2FA084", nome: "Surgimento" },
    auge:       { cor: "#E0A63C", nome: "Auge" },
    declinio:   { cor: "#B4493A", nome: "Declínio" },
    retomada:   { cor: "#2C5BA8", nome: "Retomada" }
  };
  // Escala que muda de ritmo: os séculos coloniais comprimidos, o século XX e o XXI abertos
  const QUEBRAS = [[1530, 0], [1850, 0.34], [1950, 0.57], [2026, 1]];
  const ERAS = [[1530, 1822, "Colônia"], [1822, 1889, "Império"], [1889, 1959, "República"], [1959, 2026, "Depois da SUDENE"]];

  function el(tag, attrs, pai) {
    const e = document.createElementNS(NS, tag);
    for (const k in attrs) e.setAttribute(k, attrs[k]);
    if (pai) pai.appendChild(e);
    return e;
  }
  const num = v => (v === "" || v == null ? null : +v);

  function desenhar(raiz) {
    const dados = JSON.parse(document.getElementById("dados-linha").textContent);
    const trilhas = dados.filter(d => d.tipo === "trilha").sort((a, b) => a.inicio - b.inicio);
    const trilhasF = dados.filter(d => d.tipo === "trilha_fator").sort((a, b) => a.inicio - b.inicio);
    const fases = dados.filter(d => d.tipo === "fase");
    const marcos = dados.filter(d => d.tipo === "marco");
    const fatores = dados.filter(d => d.tipo === "fator");

    const caixa = raiz.querySelector(".lt-tela");
    const dica = raiz.querySelector(".lt-dica");
    const mostrarFatores = raiz.querySelector("#lt-fatores");
    let fixa = null;

    function render() {
      caixa.innerHTML = "";
      const comF = mostrarFatores.checked;
      const W = Math.max(caixa.clientWidth, 940);
      const ESQ = 178, DIR = 18, TOPO = 46, LINHA = 34, LINHA_F = 26, SEP = comF ? 40 : 0;
      const H = TOPO + trilhas.length * LINHA + (comF ? SEP + trilhasF.length * LINHA_F : 0) + 30;
      const larg = W - ESQ - DIR;
      const x = ano => {
        for (let i = 1; i < QUEBRAS.length; i++) {
          const [a0, p0] = QUEBRAS[i - 1], [a1, p1] = QUEBRAS[i];
          if (ano <= a1 || i === QUEBRAS.length - 1) return ESQ + larg * (p0 + (p1 - p0) * (Math.min(ano, a1) - a0) / (a1 - a0));
        }
      };
      const svg = el("svg", { width: W, height: H, viewBox: `0 0 ${W} ${H}`, role: "img",
        "aria-label": "Linha do tempo das atividades econômicas de Pernambuco, de 1535 a hoje" }, caixa);
      const defs = el("defs", {}, svg);

      // eras ao fundo
      ERAS.forEach(([a, b, nome], i) => {
        el("rect", { x: x(a), y: TOPO - 30, width: x(b) - x(a), height: H - TOPO + 18, class: i % 2 ? "lt-era par" : "lt-era" }, svg);
        const t = el("text", { x: x(a) + 6, y: TOPO - 16, class: "lt-era-rot" }, svg); t.textContent = nome;
      });
      // eixo: anos de referência
      const ticks = [1535, 1600, 1700, 1800, 1850, 1900, 1950, 1975, 2000, 2026];
      ticks.forEach(a => {
        el("line", { x1: x(a), x2: x(a), y1: TOPO - 4, y2: H - 22, class: "lt-grade" }, svg);
        const t = el("text", { x: x(a), y: H - 8, class: "lt-ano", "text-anchor": "middle" }, svg); t.textContent = a === 2026 ? "hoje" : a;
      });
      // onde a escala muda de ritmo
      [1850, 1950].forEach(a => {
        el("line", { x1: x(a), x2: x(a), y1: TOPO - 30, y2: H - 22, class: "lt-quebra" }, svg);
      });

      const camadaBarras = el("g", {}, svg), camadaMarcos = el("g", {}, svg);
      const linhaY = {};
      trilhas.forEach((t, i) => {
        const y = TOPO + i * LINHA + LINHA / 2; linhaY[t.trilha] = y;
        const g = el("g", { class: "lt-rotulo", "data-trilha": t.trilha, tabindex: 0 }, svg);
        const r = el("text", { x: ESQ - 12, y: y + 4, "text-anchor": "end" }, g); r.textContent = t.titulo;
        g.addEventListener("click", () => { const c = document.getElementById(t.capitulo); if (c) c.scrollIntoView({ behavior: "smooth" }); });
        g.addEventListener("keydown", e => { if (e.key === "Enter") g.dispatchEvent(new Event("click")); });
        g.addEventListener("mouseenter", () => destacar(t.trilha)); g.addEventListener("mouseleave", () => destacar(null));
        el("line", { x1: ESQ, x2: W - DIR, y1: y, y2: y, class: "lt-trilho" }, camadaBarras);
      });

      fases.forEach((f, i) => {
        const y = linhaY[f.trilha]; if (y == null) return;
        const x0 = x(num(f.inicio)) + 1, x1 = x(num(f.fim)) - 1, cor = FASES[f.fase].cor;
        let fill = cor;
        if (f.aproximado === "inicio") {   // começo incerto: a barra surge aos poucos
          const id = "gr" + i, gr = el("linearGradient", { id, x1: 0, x2: 1, y1: 0, y2: 0 }, defs);
          el("stop", { offset: 0, "stop-color": cor, "stop-opacity": 0 }, gr);
          el("stop", { offset: 0.55, "stop-color": cor, "stop-opacity": 1 }, gr);
          fill = `url(#${id})`;
        }
        const r = el("rect", { x: x0, y: y - 6, width: Math.max(2, x1 - x0), height: 12, rx: 4, fill, class: "lt-fase", "data-trilha": f.trilha }, camadaBarras);
        alvo(r, () => ({ titulo: FASES[f.fase].nome + (f.aproximado ? " (início aproximado)" : ""), anos: `${f.inicio} a ${f.fim == 2026 ? "hoje" : f.fim}`,
                         texto: f.descricao, fonte: f.fonte, link: f.link, cor }));
      });

      marcos.forEach(m => {
        const y = linhaY[m.trilha]; if (y == null) return;
        const cx = x(num(m.inicio));
        const g = el("g", { class: "lt-marco", "data-trilha": m.trilha, tabindex: 0 }, camadaMarcos);
        el("circle", { cx, cy: y, r: 11, class: "lt-alvo" }, g);
        el("circle", { cx, cy: y, r: 5, class: "lt-ponto" }, g);
        alvo(g, () => ({ titulo: m.titulo, anos: m.fim ? `${m.inicio} a ${m.fim}` : `${m.inicio}`, texto: m.descricao, fonte: m.fonte, link: m.link }));
      });

      if (comF) {
        const y0 = TOPO + trilhas.length * LINHA + SEP;
        const t = el("text", { x: 12, y: y0 - 14, class: "lt-secao" }, svg); t.textContent = "O que empurrou e o que travou";
        el("line", { x1: 12, x2: W - DIR, y1: y0 - 8, y2: y0 - 8, class: "lt-divisa" }, svg);
        const yF = {};
        trilhasF.forEach((t, i) => {
          const y = y0 + i * LINHA_F + LINHA_F / 2; yF[t.trilha] = y;
          const r = el("text", { x: ESQ - 12, y: y + 4, "text-anchor": "end", class: "lt-rotulo-f" }, svg); r.textContent = t.titulo;
          el("line", { x1: ESQ, x2: W - DIR, y1: y, y2: y, class: "lt-trilho f" }, svg);
        });
        fatores.forEach(f => {
          const y = yF[f.trilha]; if (y == null) return;
          const a = num(f.inicio), b = num(f.fim);
          const g = el("g", { class: "lt-fator " + f.trilha, tabindex: 0 }, svg);
          if (b && x(b) - x(a) > 6) el("rect", { x: x(a), y: y - 5, width: x(b) - x(a), height: 10, rx: 3, class: "lt-faixa" }, g);
          const cx = x(a);
          el("rect", { x: cx - 10, y: y - 10, width: 20, height: 20, class: "lt-alvo" }, g);
          el("path", { d: `M${cx},${y - 6} L${cx + 6},${y} L${cx},${y + 6} L${cx - 6},${y} Z`, class: "lt-losango" }, g);
          alvo(g, () => ({ titulo: f.titulo, anos: b ? `${a} a ${b}` : `${a}`, texto: f.descricao, fonte: f.fonte, link: f.link }));
        });
      }

      // régua que acompanha o mouse
      const regua = el("line", { y1: TOPO - 30, y2: H - 22, class: "lt-regua" }, svg);
      const reguaAno = el("text", { y: TOPO - 34, class: "lt-regua-ano", "text-anchor": "middle" }, svg);
      const inv = px => {
        const p = (px - ESQ) / larg;
        for (let i = 1; i < QUEBRAS.length; i++) {
          const [a0, p0] = QUEBRAS[i - 1], [a1, p1] = QUEBRAS[i];
          if (p <= p1) return Math.round(a0 + (a1 - a0) * (p - p0) / (p1 - p0));
        }
        return 2026;
      };
      svg.addEventListener("mousemove", e => {
        const b = svg.getBoundingClientRect(), px = e.clientX - b.left;
        if (px < ESQ || px > W - DIR) { regua.style.opacity = 0; reguaAno.style.opacity = 0; return; }
        regua.setAttribute("x1", px); regua.setAttribute("x2", px); reguaAno.setAttribute("x", px);
        reguaAno.textContent = inv(px); regua.style.opacity = 1; reguaAno.style.opacity = 1;
      });
      svg.addEventListener("mouseleave", () => { regua.style.opacity = 0; reguaAno.style.opacity = 0; });

      function destacar(tr) {
        svg.querySelectorAll(".lt-fase, .lt-marco").forEach(n => n.classList.toggle("apagado", !!tr && n.dataset.trilha !== tr));
      }
    }

    function alvo(n, conteudo) {
      const mostrar = (e, fixar) => {
        const c = conteudo();
        const fontes = (c.fonte || "").split(" | "), links = (c.link || "").split(" | ");
        dica.innerHTML = `<div class="lt-dica-anos">${c.cor ? `<span class="lt-sw" style="background:${c.cor}"></span>` : ""}${c.anos}</div>
          <div class="lt-dica-tit">${c.titulo}</div><div class="lt-dica-txt">${c.texto || ""}</div>
          <div class="lt-dica-fonte">Fonte: ${fontes.map((f, i) => links[i] ? `<a href="${links[i]}" target="_blank" rel="noopener">${f}</a>` : f).join("; ")}</div>
          ${fixar ? "" : `<div class="lt-dica-ajuda">clique para fixar e abrir a fonte</div>`}`;
        const r = raiz.getBoundingClientRect(), b = n.getBoundingClientRect();
        let left = b.left - r.left + b.width / 2 + 14, top = b.top - r.top + b.height + 8;
        dica.style.display = "block";
        if (left + dica.offsetWidth > r.width - 8) left = b.left - r.left - dica.offsetWidth - 10;
        dica.style.left = Math.max(4, left) + "px"; dica.style.top = top + "px";
        dica.classList.toggle("fixa", !!fixar);
      };
      n.addEventListener("mouseenter", e => { if (!fixa) mostrar(e, false); });
      n.addEventListener("mouseleave", () => { if (!fixa) dica.style.display = "none"; });
      n.addEventListener("focus", e => { if (!fixa) mostrar(e, false); });
      n.addEventListener("blur", () => { if (!fixa) dica.style.display = "none"; });
      n.addEventListener("click", e => { e.stopPropagation(); fixa = n; mostrar(e, true); });
    }
    document.addEventListener("click", e => { if (fixa && !dica.contains(e.target)) { fixa = null; dica.style.display = "none"; } });
    document.addEventListener("keydown", e => { if (e.key === "Escape") { fixa = null; dica.style.display = "none"; } });

    mostrarFatores.addEventListener("change", render);
    let t; window.addEventListener("resize", () => { clearTimeout(t); t = setTimeout(render, 150); });
    render();
  }

  function iniciar() { document.querySelectorAll(".linha-tempo").forEach(desenhar); }
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", iniciar); else iniciar();
})();
