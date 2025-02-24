import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';

const VITE_ASSET_URL = process.env.VITE_ASSET_URL || '';

// https://vite.dev/config/
export default defineConfig({
    plugins: [react()],
    appType: 'spa',
    base: VITE_ASSET_URL,
});
