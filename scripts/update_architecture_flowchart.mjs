import fs from 'node:fs';

import path from 'node:path';

import { fileURLToPath } from 'node:url';

const PHASES = [["one","Intake & tokenization","Canonical source input"],["two","Intent & prior context","Conversation context and working memory"],["three","Lexical candidates","Dictionary and multiword alternatives"],["four","Grammar & sense competition","LIFG, ATL, grammar and structure"],["five","Semantic construction","Meaning composition and integration audits"],["six","Comprehension & speech acts","Meaning summaries and speech acts"],["seven","WM & response planning","Working memory and action selection"],["eight","Persistence & feedback","Qualified experience and feedback"]];

function moduleCatalog(paths){
 const map=new Map([["Core","apps/core/lib/core.ex"]]),caps=new Set(["atl","lifg","stm","ltm","mwe","wm","acc","db"]);
 for(const p of paths){
  const m=/^apps\/[^/]+\/lib\/(.+)\.ex$/.exec(p);if(!m||m[1].startsWith("mix/"))continue;
  map.set(m[1].split("/").map(seg=>seg.split("_").map(s=>caps.has(s)?s.toUpperCase():s[0]?.toUpperCase()+s.slice(1)).join("")).join("."),p);
 }return map;
}

function sourceStages(src,mods){
 const aliases=new Map([...src.matchAll(/^\s*alias\s+([A-Z][\w.]*)\s*(?:,\s*as:\s*([A-Z]\w*))?/gm)].map(m=>[m[2]||m[1].split(".").at(-1),m[1]]));
 return PHASES.map(([word,title,description],k)=>{
  const m=new RegExp(`^  defp phase_${word}\\([^\\n]*\\) do\\s*$`,"m").exec(src);
  if(!m)throw Error("Missing Core.phase_"+word+"; no partial diagram written");
  const a=m.index+m[0].length, next=/^  defp?\s+[a-z_]/gm;next.lastIndex=a;
  const e=next.exec(src),body=src.slice(a,e?e.index:src.length),calls=[];
  if(word==="one"&&body.includes("Core.Text.Fuzzy.interpret(")&&mods.has("Core.Text.Fuzzy"))calls.push({i:body.indexOf("Core.Text.Fuzzy.interpret("),mod:"Core.Text.Fuzzy",method:"interpret",status:"conditional"});
  for(const match of body.matchAll(/\|>\s*((?:[A-Z][\w]*(?:\.[A-Z][\w]*)*\.)?[a-z][\w!?]*)\s*\(/g)){
   const name=match[1],dot=name.lastIndexOf(".");
   let mod=dot<0?"Core":name.slice(0,dot),method=dot<0?name:name.slice(dot+1);
   if(aliases.has(mod))mod=aliases.get(mod);
   if(mod==="Core"&&method==="apply_intent_selection"){mod="Core.Intent.Selection";method="select";}
   if(mod==="Core.StateDump"||mod==="Map"||(mod==="Core"&&["maybe_put_fuzzy","pipeline_map"].includes(method)))continue;
   if(!mods.has(mod))continue;
   const status=word==="two"&&body.includes(":prod ->")&&match.index>body.indexOf(":prod ->")?"production branch":"direct call";
   calls.push({i:match.index,mod,method,status});
  }
  calls.sort((a,b)=>a.i-b.i);if(!calls.length)throw Error("No resolved source calls in phase_"+word);
  return {key:word,title,description,number:k+1,stages:calls.map((c,j)=>({
   id:`p${k+1}s${j+1}`,phase:k+1,order:j+1,module:c.mod,function:c.method,label:`${c.mod}.${c.method}`,
   description:c.status==="production branch"?"Call within production-only branch":c.status==="conditional"?"Conditionally invoked":"Pipeline call (implementation may contain guards)",
   status:c.status,file:mods.get(c.mod)}))};
 });
}

function escapeHtml(v){return String(v).replace(/&/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;").replace(/"/g,"&quot;");}

function svg(items,arrows=true){
 const height=Math.max(110,items.length*76+26),p=[
 `<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" viewBox="0 0 860 ${height}" role="img" aria-label="Source-derived diagram">`,
 `<rect width="860" height="${height}" fill="#f5faff"/>`];
 items.forEach((x,i)=>{
  const y=16+i*76;
  if(arrows&&i)p.push(`<path d="M430 ${y-20} V${y-2}" stroke="#5f819d" stroke-width="2" fill="none"/>`,`<path d="M425 ${y-8} L430 ${y} L435 ${y-8}" fill="none" stroke="#5f819d" stroke-width="2"/>`);
  const title=x.label.length>86?x.label.slice(0,83)+"...":x.label,fill=x.status==="source-only"?"#fff4df":"#e1f1ff";
  const shape=`<rect x="55" y="${y}" width="750" height="53" rx="9" fill="${fill}" stroke="#63809c" stroke-width="1.5"/><text x="74" y="${y+23}" fill="#163b57" font-size="14" font-weight="600" font-family="system-ui, sans-serif">${escapeHtml(title)}</text><text x="74" y="${y+40}" fill="#54738a" font-size="11" font-family="system-ui, sans-serif">${escapeHtml(x.status||"")}</text>`;
  p.push(x.id?`<a xlink:href="#inspect:${escapeHtml(x.id)}">${shape}</a>`:shape);
 });return p.concat("</svg>").join("\n");
}

function between(s,start,end,replacement){
 const a=s.indexOf(start);
 if(a<0||s.indexOf(start,a+start.length)!==-1)throw Error("Missing or duplicated template marker: "+start);
 const b=s.indexOf(end,a+start.length);if(b<0)throw Error("Missing template end marker: "+end);
 return s.slice(0,a)+replacement+s.slice(b);
}

function view(id,title,description,items,arrows=true){
 return `<section class="diagram" data-view="${escapeHtml(id)}" style="display:none"><div class="diagram-head"><h2>${escapeHtml(title)}</h2><p>${escapeHtml(description)}</p></div><div class="canvas">${svg(items,arrows)}</div></section>`;
}

function regenerate(original,source,paths,branchName="symbolic_generation"){
 const modules=moduleCatalog(paths),phases=sourceStages(source,modules),all=phases.flatMap(p=>p.stages),called=new Set(all.map(s=>s.module));
 const item=(mod,status,i,prefix)=>({id:`${prefix}${i+1}`,module:mod,label:mod,status,file:modules.get(mod),description:status==="source-only"?"Source-present module; direct production call not established":"Direct main-pipeline call"});
 const evidence=[...modules.keys()].filter(s=>s.startsWith("Core.Integration.")||/^Core\.(?:Semantic|Text)\.(?:Passage|NamedMentions|PassageAntecedents)(?:\.|$)/.test(s))
  .sort((a,b)=>Number(called.has(b))-Number(called.has(a))||a.localeCompare(b)).map((s,i)=>item(s,called.has(s)?"direct call":"source-only",i,"ev"));
 const brain=all.filter(s=>/(?:Brain|LIFG|ATL|Amygdala|Prefrontal)/.test(s.module)).map((s,i)=>({...s,id:`brain${i+1}`,label:`Phase ${s.phase}: ${s.label}`}));
 const response=[...modules.keys()].filter(x=>x.startsWith("Core.Response.")).sort().slice(0,36).map((s,i)=>item(s,called.has(s)?"direct call":"source-only",i,"resp"));
 const overview=phases.map((p,i)=>({id:`o${i+1}`,label:`Phase ${p.number} — ${p.title} (${p.stages.length} mapped calls)`,module:"Core",status:"ordered source phase",description:p.description,file:"apps/core/lib/core.ex"}));
 const runtime=[{id:"runtime0",label:"Core.resolve_input → phase_one → phase_two",module:"Core",status:"source-defined intake",description:"Intake before production dispatch",file:"apps/core/lib/core.ex"},...overview.slice(2).map((v,i)=>({...v,id:`runtime${i+1}`}))];
 const lookup=Object.fromEntries([...all,...overview,...runtime,...brain,...response,...evidence].map(v=>[v.id,{...v,source_url:`https://github.com/bushman77/Symbrella/blob/${encodeURIComponent(branchName)}/${v.file}`}]));
 const views=[view("overview","System overview","Phase boundaries from Core.run_prod_pipeline",overview),view("runtime","Chat execution & branching","Source-ordered phases; not a measured runtime trace",runtime),view("brain","Brain integrations","Brain-related calls observed in the main pipeline",brain),view("response","Response modules","Available modules; source-only is not proof of invocation",response,false),view("evidence","Semantic evidence & observers","Read-only observers distinguished from pipeline call sites",evidence,false),...phases.map(p=>view(`p${p.number}`,`Phase ${p.number}: ${p.title}`,p.description,p.stages))].join("\n");
 const details=`<h2 class="details-title">Complete ordered pipeline stages</h2><p style="color:#a9c3d7;margin:-5px 0 12px">Extracted from Core.ex; StateDump captures omitted for readability. Click any step for source.</p><input id="stage-search" placeholder="Search modules, stages, status…" autocomplete="off">`
  +phases.map(p=>`<details class="phase-detail" id="detail-p${p.number}"><summary><b>${p.number}. ${escapeHtml(p.title)}</b><span>${p.stages.length} mapped calls</span></summary><div class="stage-list">`
   +p.stages.map(s=>`<button class="stage-row" data-inspect="${s.id}"><span class="order">${String(s.order).padStart(2,"0")}</span><span><b>${escapeHtml(s.label)}</b><small>${escapeHtml(s.description)}</small></span><em>${escapeHtml(s.status)}</em></button>`).join("")
   +"</div></details>").join("");
 const nav=[["overview","System overview"],["runtime","Chat execution & branching"],["brain","Brain integrations"],["response","Response modules"],["evidence","Semantic evidence & observers"],...phases.map(p=>[`p${p.number}`,`Phase ${p.number}: ${p.title}`])].map(([key,label])=>`<button class="nav-btn" data-open="${escapeHtml(key)}">${escapeHtml(label)}</button>`).join("");
 let s=original;
 s=between(s,"<header>","</header>",`<header><div><h1>SYMBRELLA <span style="color:#6ee2cf">/ Architecture Explorer</span></h1><div class="subtitle">Source-derived architecture · ${escapeHtml(branchName)} · local working tree</div></div><div class="stats"><span class="stat"><b>8</b> phases</span><span class="stat"><b>${all.length}</b> mapped calls</span><span class="stat"><b>${modules.size}</b> source modules</span><span class="chip">READ-ONLY STATIC AUDIT</span></div>`);
 s=between(s,'<aside class="left">',"</aside>",`<aside class="left"><h3 class="sidebar-title">Explore diagrams</h3>${nav}<div class="info-card"><h3>Source snapshot</h3><p>Generated from local source, not measured runtime execution. Links target <a href="https://github.com/bushman77/Symbrella/tree/${escapeHtml(branchName)}" target="_blank">${escapeHtml(branchName)}</a> after push.</p></div>`);
 s=between(s,'<section class="diagram" data-view="overview"','<h2 class="details-title">Complete ordered pipeline stages</h2>',views+"\n");
 s=between(s,'<h2 class="details-title">Complete ordered pipeline stages</h2>',"</main>",details);
 s=between(s,"<footer>","</footer>",`<footer>Source-derived Symbrella architecture · <a href="https://github.com/bushman77/Symbrella/tree/${escapeHtml(branchName)}">${escapeHtml(branchName)}</a> · Static call-site inventory, not a runtime trace.`);
 const tail=s.includes(";\nlet active='overview'")?";\nlet active='overview'":";let active='overview'";
 s=between(s,"<script>const LOOKUP = ",tail,`<script>const LOOKUP = ${JSON.stringify(lookup)}`);
 s=s.replace("linked to the requested commit","linked to the selected branch").replace("exist at the commit","exist in the source tree");
 if(!s.includes('data-open="evidence"')||!s.includes("Core.Integration.Audit"))throw Error("Generated explorer failed validation");
 return s;
}

function collectPaths(root,fs,path){
 const out=[];
 function walk(rel){for(const e of fs.readdirSync(path.join(root,rel),{withFileTypes:true})){const next=path.posix.join(rel,e.name);if(e.isDirectory())walk(next);else if(e.isFile())out.push(next);}}
 walk("apps");return out;
}

export {moduleCatalog, sourceStages, regenerate};

const here=path.resolve(path.dirname(fileURLToPath(import.meta.url)),"..");

if(process.argv[1] && path.resolve(process.argv[1])===fileURLToPath(import.meta.url)){

  const dest=path.join(here,"docs/flowcharts/symbrella_architecture.html");

  const source=fs.readFileSync(path.join(here,"apps/core/lib/core.ex"),"utf8");

  const old=fs.readFileSync(dest,"utf8");

  const output=regenerate(old,source,collectPaths(here,fs,path),process.env.SYMBRELLA_ARCH_BRANCH||"symbolic_generation");

  if(output===old)console.log("Architecture Explorer already current.");

  else if(process.argv.includes("--check")){console.error("Architecture Explorer stale; run its generator.");process.exitCode=1;}

  else{const temp=dest+"."+process.pid+".tmp";try{fs.writeFileSync(temp,output);fs.renameSync(temp,dest);}finally{if(fs.existsSync(temp))fs.unlinkSync(temp);}console.log("Architecture Explorer refreshed from source.");}

}
