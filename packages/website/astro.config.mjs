// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';
import starlightNextjsTheme from "starlight-nextjs-theme";

const data = await fetch(
  "https://raw.githubusercontent.com/thomasvergne/bonzai/refs/heads/language-reworking/packages/vscode/syntaxes/bonzai.tmLanguage.json"
);
const json = await data.json();

// https://astro.build/config
export default defineConfig({
  site: "https://bonzai.thomas-vergne.fr",
  markdown: {
    shikiConfig: {
      langs: [
        {
          ...json,
          aliases: ["bonzai"],
        },
      ],
    },
  },
  integrations: [
    starlight({
      plugins: [starlightNextjsTheme()],
      title: "Bonzai",
      social: [
        {
          href: "https://github.com/thomasvergne/bonzai",
          icon: "github",
          label: "GitHub",
        },
      ],
      sidebar: [
        {
          label: "Guides",
          autogenerate: { directory: "guides" },
        },
        {
          label: "Reference",
          autogenerate: { directory: "reference" },
        },
      ],
      credits: false,
      defaultLocale: "en",
      description:
        "Bonzai is a general purpose programming language. It compiles down to a custom bytecode.",
      head: [
        // Add SEO images
        {
          tag: "meta",
          attrs: {
            property: "twitter:image",
            content: "/banner.png",
          },
        },
        {
          tag: "meta",
          attrs: {
            property: "og:image",
            content: "/banner.png",
          },
        },
      ],
    }),
  ],
});
