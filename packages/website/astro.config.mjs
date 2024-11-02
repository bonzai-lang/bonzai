// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';
import tailwind from '@astrojs/tailwind';


const data = await fetch('https://raw.githubusercontent.com/thomasvergne/bonzai/refs/heads/main/packages/vscode/syntaxes/bonzai.tmLanguage.json');
const json = await data.json();

import ShikiTheme from 'shiki/themes/github-dark-default.mjs';

// https://astro.build/config
export default defineConfig({
  site: 'https://bonzai.thomas-vergne.fr',
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
  },
	integrations: [
		starlight({
			title: 'Bonzai',
			social: {
				github: 'https://github.com/thomasvergne/bonzai',
			},
			sidebar: [
				{
					label: 'Guides',
          autogenerate: { directory: 'guides' },
				},
				{
					label: 'Reference',
					autogenerate: { directory: 'reference' },
				},
			],
			customCss: ['./src/tailwind.css'],
      credits: false,
      favicon: '/logo.svg',
      defaultLocale: 'en',
      description: 'Bonzai is a programming language based on actor models. It compiles down to a custom bytecode.',
      head: [
        // Add SEO images
        {
          tag: 'meta',
          attrs: {
            property: 'twitter:image',
            content: '/banner.png',
          },
        },
        {
          tag: 'meta',
          attrs: {
            property: 'og:image',
            content: '/banner.png',
          },
        },
      ]
		}),
		tailwind({ applyBaseStyles: false }),
	],
});
