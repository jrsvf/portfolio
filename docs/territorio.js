// Blocos do território usados no raio-X dos municípios e das regiões.
// Cada função recebe o pedaço de dados e as ferramentas do OJS ({Plot, html})
// e devolve um elemento pronto para a página (ou null quando não há dado).

const nf = (x, d = 0) => x == null || isNaN(x) ? "–" : x.toLocaleString("pt-BR", {maximumFractionDigits: d, minimumFractionDigits: d});
const pct = (x, d = 1) => x == null || isNaN(x) ? "–" : (100 * x).toLocaleString("pt-BR", {maximumFractionDigits: d, minimumFractionDigits: d}) + "%";
const reais = x => x == null ? "–" : x >= 1e9 ? "R$ " + nf(x / 1e9, 2) + " bi" : x >= 1e6 ? "R$ " + nf(x / 1e6, 1) + " mi" : "R$ " + nf(x / 1e3, 0) + " mil";
const AZUL = "#1F4E79", LAR = "#E07A1F", CINZA = "#B8C4CF";

export function cartoes(html, itens) {
  return html`<div class="cartoes">${itens.filter(Boolean).map(([rot, val, nota]) =>
    html`<div class="cartao"><div class="cartao-valor">${val}</div><div class="cartao-rotulo">${rot}</div>${nota ? html`<div class="cartao-nota">${nota}</div>` : ""}</div>`)}</div>`;
}

export function blocoEmpresas(e, {Plot, html}, largura = 640) {
  if (!e || !e.serie || !e.serie.length) return html`<p class="aviso">Sem dados do cadastro de empresas.</p>`;
  const longo = e.serie.flatMap(d => [
    {ano: d.ano, tipo: "Aberturas (sem MEI)", v: d.aberturas - d.aberturas_mei},
    {ano: d.ano, tipo: "Aberturas de MEI", v: d.aberturas_mei},
    {ano: d.ano, tipo: "Baixas", v: -d.baixas}]);
  const u = e.serie[e.serie.length - 1];
  return html`<div>
    ${cartoes(html, [
      ["Empresas ativas (sem MEI)", nf(e.ativas - e.ativas_mei), "Cadastro da Receita de " + e.data_base],
      ["MEI ativos", nf(e.ativas_mei), ""],
      ["Aberturas em " + u.ano, nf(u.aberturas), nf(u.aberturas_mei) + " MEI"],
      ["Baixas em " + u.ano, nf(u.baixas), "Saldo: " + nf(u.aberturas - u.baixas)]])}
    <div class="rx-duas">
      <div><h4>Aberturas e baixas por ano</h4>${Plot.plot({
        width: largura, height: 260, marginLeft: 56, x: {label: null, tickFormat: d => String(d)},
        y: {label: "estabelecimentos", grid: true, tickFormat: d => nf(d)},
        color: {domain: ["Aberturas (sem MEI)", "Aberturas de MEI", "Baixas"], range: [AZUL, "#7FA7CF", LAR], legend: true},
        marks: [Plot.barY(longo, {x: "ano", y: "v", fill: "tipo", tip: true}), Plot.ruleY([0])]})}</div>
      <div><h4>Empresas ativas por setor (sem MEI)</h4><table class="rx-tab"><tbody>
        ${e.por_secao.map(s => html`<tr><td>${s.setor}</td><td>${nf(s.ativas)}</td></tr>`)}</tbody></table></div>
    </div></div>`;
}

export function blocoQualif(q, {Plot, html}, largura = 640) {
  if (!q) return html`<p class="aviso">Sem dados de escolaridade.</p>`;
  const ordem = ["Até fundamental", "Médio", "Superior", "Pós-graduação", "Não informado"];
  return html`<div>
    ${cartoes(html, [
      ["Vínculos formais com ensino superior em " + q.ano, pct(q.superior), pct(q.superior0) + " em " + q.ano0],
      ["Vínculos formais em " + q.ano, nf(q.total), "RAIS"]])}
    <div class="rx-duas">
      <div><h4>Escolaridade dos vínculos formais</h4>${Plot.plot({
        width: largura, height: 90, marginLeft: 10, x: {label: null, tickFormat: d => (100 * d).toFixed(0) + "%", domain: [0, 1]},
        color: {domain: ordem, range: ["#C9D6E3", "#7FA7CF", AZUL, LAR, "#E5EAF0"], legend: true},
        y: {axis: null}, marks: [Plot.barX(q.escolaridade, Plot.stackX({x: "p", y: () => "esc", fill: "escolaridade", order: ordem, title: d => d.escolaridade + ": " + pct(d.p)}))]})}</div>
      <div><h4>Ocupações (grandes grupos da CBO)</h4><table class="rx-tab"><tbody>
        ${q.ocupacao.slice(0, 8).map(o => html`<tr><td>${o.ocupacao}</td><td>${nf(o.v)}</td><td>${pct(o.p)}</td></tr>`)}</tbody></table></div>
    </div></div>`;
}

export function blocoEducacao(ed, {Plot, html}, largura = 640) {
  if (!ed || (!ed.matriculas && !(ed.tecnico && ed.tecnico.matriculas))) return html`<p>Não há oferta presencial de ensino superior nem de educação profissional registrada aqui.</p>`;
  return html`<div>
    ${cartoes(html, [
      ed.matriculas ? ["Matrículas no ensino superior presencial (" + ed.ano + ")", nf(ed.matriculas), pct(ed.publica / ed.matriculas, 0) + " em instituições públicas"] : null,
      ed.concluintes ? ["Concluintes em " + ed.ano, nf(ed.concluintes), nf(ed.cursos) + " cursos"] : null,
      ed.tecnico && ed.tecnico.matriculas ? ["Matrículas em educação profissional (" + ed.tecnico.ano + ")", nf(ed.tecnico.matriculas), nf(ed.tecnico.escolas) + " escolas"] : null])}
    <div class="rx-duas">
      ${ed.areas && ed.areas.length ? html`<div><h4>Matrículas por área (ensino superior presencial)</h4>${Plot.plot({
        width: largura, height: 26 * Math.min(ed.areas.length, 10) + 30, marginLeft: 250, x: {label: null, grid: true, tickFormat: d => nf(d)}, y: {label: null},
        marks: [Plot.barX(ed.areas.slice(0, 10), {y: "area", x: "matriculas", fill: AZUL, sort: {y: "-x"}, tip: true})]})}</div>` : ""}
      ${ed.ies && ed.ies.length ? html`<div><h4>Instituições de ensino superior</h4><table class="rx-tab"><thead><tr><th>Instituição</th><th>Tipo</th><th>Matrículas</th></tr></thead><tbody>
        ${ed.ies.slice(0, 12).map(i => html`<tr><td>${i.sigla ? i.sigla + " · " : ""}${i.nome}</td><td>${i.categoria ?? ""}</td><td>${i.matriculas ? nf(i.matriculas) : "–"}</td></tr>`)}</tbody></table></div>` : ""}
    </div></div>`;
}

export function blocoIcms(ic, {Plot, html}, largura = 640) {
  if (!ic || !ic.serie || !ic.serie.length) return html`<p class="aviso">Sem dados de cota-parte do ICMS.</p>`;
  return html`<div>
    ${cartoes(html, [
      ["Cota-parte do ICMS em " + ic.ano, reais(ic.valor), pct(ic.part_estado, 2) + " do repasse do estado aos municípios"],
      ["Por habitante", ic.per_capita ? "R$ " + nf(ic.per_capita) : "–", ""]])}
    ${Plot.plot({width: largura, height: 220, marginLeft: 70, x: {label: null, tickFormat: d => String(d)},
      y: {label: "R$ milhões (nominais)", grid: true, tickFormat: d => nf(d / 1e6)},
      marks: [Plot.barY(ic.serie, {x: "ano", y: "valor", fill: AZUL, tip: true}), Plot.ruleY([0])]})}</div>`;
}

export function blocoAgro(ag, {Plot, html}, largura = 640) {
  if (!ag || (!ag.censo && !(ag.pronaf && ag.pronaf.length))) return html`<p class="aviso">Sem dados de agricultura familiar.</p>`;
  const c = ag.censo;
  return html`<div>
    ${c && c.estab_tot ? cartoes(html, [
      ["Estabelecimentos da agricultura familiar (2017)", pct(c.estab_fam / c.estab_tot, 0), nf(c.estab_fam) + " de " + nf(c.estab_tot)],
      ["Da área dos estabelecimentos", pct(c.area_fam / c.area_tot, 0), nf(c.area_fam) + " ha"],
      ["Do valor da produção", pct(c.vp_fam / c.vp_tot, 0), reais(1000 * c.vp_fam)]]) : ""}
    ${ag.pronaf && ag.pronaf.length ? html`<h4>Crédito do PRONAF contratado por ano</h4>${Plot.plot({width: largura, height: 220, marginLeft: 70,
      x: {label: null, tickFormat: d => String(d)}, y: {label: "R$ milhões (nominais)", grid: true, tickFormat: d => nf(d / 1e6)},
      marks: [Plot.barY(ag.pronaf, {x: "ano", y: "valor", fill: "#5B8C5A", tip: true}), Plot.ruleY([0])]})}` : ""}</div>`;
}

export function blocoInfra(inf, {html}) {
  if (!inf || !inf.length) return html`<p>Nenhum porto, aeroporto, polo ou ferrovia da lista curada fica aqui.</p>`;
  return html`<table class="rx-tab rx-infra"><tbody>${inf.map(i => html`<tr><td><b>${i.tipo}</b></td><td>${i.nome}</td><td>${i.nota ?? ""}</td></tr>`)}</tbody></table>`;
}

const PORTE = {"Micro Empresa": "microempresa", "Empresa De Pequeno Porte": "pequeno porte", "Demais": "médio ou grande porte"};
export function listaMaiores(lst, {html}) {
  if (!lst || !lst.length) return "";
  return html`<div class="rx-cc-bloco"><h5>Maiores empresas ativas (capital social)</h5><ul class="rx-emp">
    ${lst.map(e => html`<li><b>${e.nome_fantasia || e.razao_social}</b>
      <span>${[e.atividade, PORTE[e.porte] ?? e.porte, e.abertura ? "desde " + e.abertura : null].filter(Boolean).join(" · ")}</span></li>`)}</ul></div>`;
}
