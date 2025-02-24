import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';

const ASSET_URL = process.env.ASSET_URL || '';

// https://vite.dev/config/
export default defineConfig({
    plugins: [react()],
    appType: 'spa',
    base: ASSET_URL,
});
