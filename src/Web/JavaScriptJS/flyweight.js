class StyleFactory {
  constructor(){ this.styles = new Map(); }
  get(font,size,color){ const key=`${font}|${size}|${color}`; if(!this.styles.has(key)) this.styles.set(key,Object.freeze({font,size,color})); return this.styles.get(key); }
}
const f=new StyleFactory(); const r1=f.get('Inter',12,'red'); const r2=f.get('Inter',12,'red'); f.get('Inter',12,'blue');
console.log(`styles=${f.styles.size};shared=${r1===r2};text=ABC`);
