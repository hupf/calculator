if(!self.define){let e,i={};const n=(n,s)=>(n=new URL(n+".js",s).href,i[n]||new Promise((i=>{if("document"in self){const e=document.createElement("script");e.src=n,e.onload=i,document.head.appendChild(e)}else e=n,importScripts(n),i()})).then((()=>{let e=i[n];if(!e)throw new Error(`Module ${n} didn’t register its module`);return e})));self.define=(s,c)=>{const r=e||("document"in self?document.currentScript.src:"")||location.href;if(i[r])return;let o={};const d=e=>n(e,r),f={module:{uri:r},exports:o,require:d};i[r]=Promise.all(s.map((e=>f[e]||d(e)))).then((e=>(c(...e),o)))}}define(["./workbox-5d567057"],(function(e){"use strict";self.skipWaiting(),e.clientsClaim(),e.precacheAndRoute([{url:"android-chrome-192x192.png",revision:"4e5c8bf32badf7860be518477657eb37"},{url:"android-chrome-512x512.png",revision:"b65582f70071be9f3c48daef257609f1"},{url:"apple-touch-icon.png",revision:"8b7eee3b5d6ac1da028dc6597241b31c"},{url:"assets/index.3db043d8.js",revision:null},{url:"assets/index.71dd4e26.css",revision:null},{url:"browserconfig.xml",revision:"d527e3f39fac8ea7c5f4fba27055a753"},{url:"favicon-16x16.png",revision:"588e85beac548d081c1ea27b03c7b25b"},{url:"favicon-32x32.png",revision:"165ad34f755dfa79d2ddafe9912dba3b"},{url:"favicon.ico",revision:"9dd9060613c7c31b5d022d34ed8687f6"},{url:"icon.svg",revision:"b8721e5b02d01f266c6ea6f19a7abc43"},{url:"index.html",revision:"89a75d9c0a9bb42e9c9d0f02ef265231"},{url:"mstile-144x144.png",revision:"5e02f39efdd265eaeb90f88ddd9538b9"},{url:"mstile-150x150.png",revision:"12837ffdd7b93cfbb9535d772e0c440e"},{url:"mstile-310x150.png",revision:"74e7f2457be87632ff3d621512b3456d"},{url:"mstile-310x310.png",revision:"5293ae5c75312fddfb74caec0ab3f5a7"},{url:"mstile-70x70.png",revision:"fe49d44bf01b739f8f89c8e63ce97bd6"},{url:"registerSW.js",revision:"7a3217f0fb7ae3b80cf5889c9f24ccbe"},{url:"safari-pinned-tab.svg",revision:"14cae5e1fee392b3f888d5926bc16ea0"},{url:"manifest.webmanifest",revision:"a8bcb691914d18259f2b670598a7bc78"}],{}),e.cleanupOutdatedCaches(),e.registerRoute(new e.NavigationRoute(e.createHandlerBoundToURL("index.html"))),e.registerRoute(/^https:\/\/fonts\.googleapis\.com\/.*/i,new e.CacheFirst({cacheName:"google-fonts-cache",plugins:[new e.ExpirationPlugin({maxEntries:10,maxAgeSeconds:31536e3}),new e.CacheableResponsePlugin({statuses:[0,200]})]}),"GET"),e.registerRoute(/^https:\/\/fonts\.gstatic\.com\/.*/i,new e.CacheFirst({cacheName:"gstatic-fonts-cache",plugins:[new e.ExpirationPlugin({maxEntries:10,maxAgeSeconds:31536e3}),new e.CacheableResponsePlugin({statuses:[0,200]})]}),"GET")}));
