import colors from 'tailwindcss/colors';
import starlightPlugin from '@astrojs/starlight-tailwind';

/** @type {import('tailwindcss').Config} */
export default {
	content: ['./src/**/*.{astro,html,js,jsx,md,mdx,svelte,ts,tsx,vue}'],
	theme: {
		extend: {
			colors: {
				// Your preferred accent color. Indigo is closest to Starlight’s defaults.
				accent: {
          '50': '#ebf8ff',
          '100': '#d1f1ff',
          '200': '#aee7ff',
          '300': '#76daff',
          '400': '#35c3ff',
          '500': '#079fff',
          '600': '#0079ff',
          '700': '#0060ff',
          '800': '#004fd7',
          '900': '#004aad',
          '950': '#062c65',
        },
				// Your preferred gray scale. Zinc is closest to Starlight’s defaults.
				gray: colors.zinc,
			},
		},
	},
	plugins: [starlightPlugin()],
};
