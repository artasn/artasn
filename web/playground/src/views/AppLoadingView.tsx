import React from 'react';
import LoadingSpinner from '../components/LoadingSpinner';

export interface AppLoadingViewProps {
    loading: boolean;
}

const AppLoadingView = ({ loading, ref }: AppLoadingViewProps & React.RefAttributes<null>) => (
    <div ref={ref} style={{
        background: '#ffffff',
        zIndex: 1000,
        position: 'fixed',
        left: 0,
        top: 0,
        width: '100vw',
        height: '100vh',
        display: 'flex',
        flexDirection: 'column',
        justifyContent: 'center',
        textAlign: 'center',
        transition: 'opacity 0.5s linear',
        opacity: loading ? 1 : 0,
        pointerEvents: 'none',
    }}>
        {loading && (
            <LoadingSpinner size={120} />
        )}
    </div>
);

export default AppLoadingView;
