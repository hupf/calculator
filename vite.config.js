import { defineConfig } from "vite";
import ViteElm from "vite-plugin-elm";
import { VitePWA } from "vite-plugin-pwa";

const basePath =
  process.argv.find((a) => a.startsWith("--base"))?.split("=")[1] || "/";

export default defineConfig({
  plugins: [
    ViteElm(),
    VitePWA({
      registerType: "autoUpdate",
      workbox: {
        globPatterns: ["**/*{js,css,html,xml,ico,png,svg}"],
        runtimeCaching: [
          // Cache Google Fonts
          {
            urlPattern: /^https:\/\/fonts\.googleapis\.com\/.*/i,
            handler: "CacheFirst",
            options: {
              cacheName: "google-fonts-cache",
              expiration: {
                maxEntries: 10,
                maxAgeSeconds: 60 * 60 * 24 * 365, // 365 days
              },
              cacheableResponse: {
                statuses: [0, 200],
              },
            },
          },
          {
            urlPattern: /^https:\/\/fonts\.gstatic\.com\/.*/i,
            handler: "CacheFirst",
            options: {
              cacheName: "gstatic-fonts-cache",
              expiration: {
                maxEntries: 10,
                maxAgeSeconds: 60 * 60 * 24 * 365, // 365 days
              },
              cacheableResponse: {
                statuses: [0, 200],
              },
            },
          },
        ],
      },
      manifest: {
        name: "Calculator",
        short_name: "Calculator",
        description: "A Dieter Rams inspired calculator implemented in Elm",
        display: "standalone",
        theme_color: "#5bbad5",
        background_color: "#5bbad5",
        start_url: basePath,
        scope: basePath,
        icons: [
          {
            src: `${basePath}android-chrome-192x192.png`,
            sizes: "192x192",
            type: "image/png",
          },
          {
            src: `${basePath}/android-chrome-512x512.png`,
            sizes: "512x512",
            type: "image/png",
          },
        ],
      },
    }),
  ],
});
