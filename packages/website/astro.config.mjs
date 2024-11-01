// @ts-check
import { defineConfig } from 'astro/config';
import mdx from '@astrojs/mdx';

import sitemap from '@astrojs/sitemap';

import tailwind from '@astrojs/tailwind';

import icon from 'astro-icon';

const data = await fetch('https://raw.githubusercontent.com/thomasvergne/bonzai/refs/heads/main/packages/vscode/syntaxes/bonzai.tmLanguage.json');
const json = await data.json();

import ShikiTheme from 'shiki/themes/github-dark-default.mjs';

// https://astro.build/config
export default defineConfig({
    site: 'https://example.com',
    integrations: [mdx(), sitemap(), tailwind(), icon()],
    markdown: {
      shikiConfig: {
        theme: {
          ...ShikiTheme,
          bg: '#0a0a0a',
        },
        langs: [
          {
            ...json,
            aliases: ['bonzai'],
          }
        ]
      }
    }
});