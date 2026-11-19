import React, { useCallback, useEffect, useRef, useState } from 'react';

export interface SplitDividerProps {
    /** position of the divider, as a percentage of the window width */
    position: number;
    onChange: (position: number) => void;
    /** called when the divider is double-clicked */
    onReset?: () => void;
    /** minimum width of each side, in pixels */
    minSideWidth?: number;
}

/** width of the area that can be grabbed with the mouse */
const HIT_WIDTH = 8;
/** amount the divider moves per arrow key press, as a percentage of the window width */
const KEYBOARD_STEP = 2;
/** maximum time between two clicks for them to count as a double-click, in milliseconds */
const DOUBLE_CLICK_MS = 400;

export function clampSplitPosition(position: number, minSideWidth: number): number {
    const minPercent = Math.min(50, (minSideWidth / window.innerWidth) * 100);
    return Math.min(100 - minPercent, Math.max(minPercent, position));
}

/**
 * A vertical bar that can be dragged left/right to resize two side-by-side, fixed-position panes.
 */
const SplitDivider = ({ position, onChange, onReset, minSideWidth = 300 }: SplitDividerProps) => {
    const [dragging, setDragging] = useState(false);
    const [hovering, setHovering] = useState(false);
    const onChangeRef = useRef(onChange);
    onChangeRef.current = onChange;

    const updateFromClientX = useCallback((clientX: number) => {
        const percent = (clientX / window.innerWidth) * 100;
        onChangeRef.current(clampSplitPosition(percent, minSideWidth));
    }, [minSideWidth]);

    useEffect(() => {
        if (!dragging) {
            return;
        }

        const onPointerMove = (e: PointerEvent) => updateFromClientX(e.clientX);
        const onPointerUp = () => setDragging(false);
        window.addEventListener('pointermove', onPointerMove);
        window.addEventListener('pointerup', onPointerUp);
        window.addEventListener('pointercancel', onPointerUp);
        return () => {
            window.removeEventListener('pointermove', onPointerMove);
            window.removeEventListener('pointerup', onPointerUp);
            window.removeEventListener('pointercancel', onPointerUp);
        };
    }, [dragging, updateFromClientX]);

    const lastPointerDown = useRef(0);
    const onPointerDown = (e: React.PointerEvent) => {
        if (e.button !== 0) {
            return;
        }
        e.preventDefault();

        // Detect double-clicks here rather than with onDoubleClick: the first click starts a drag, which shows the
        // full-page overlay, so the second click would land on the overlay instead of the divider.
        const now = e.timeStamp;
        if (now - lastPointerDown.current < DOUBLE_CLICK_MS) {
            lastPointerDown.current = 0;
            onReset?.();
            return;
        }
        lastPointerDown.current = now;

        setDragging(true);
    };

    const onKeyDown = (e: React.KeyboardEvent) => {
        let delta = 0;
        if (e.key === 'ArrowLeft') {
            delta = -KEYBOARD_STEP;
        } else if (e.key === 'ArrowRight') {
            delta = KEYBOARD_STEP;
        } else {
            return;
        }
        e.preventDefault();
        onChange(clampSplitPosition(position + delta, minSideWidth));
    };

    const active = dragging || hovering;

    return (
        <>
            <div
                role="separator"
                aria-orientation="vertical"
                aria-valuenow={Math.round(position)}
                aria-valuemin={0}
                aria-valuemax={100}
                aria-label="Resize editor"
                tabIndex={0}
                onPointerDown={onPointerDown}
                onPointerEnter={() => setHovering(true)}
                onPointerLeave={() => setHovering(false)}
                onKeyDown={onKeyDown}
                style={{
                    // same z-index as the VSCode container, but rendered after it, so it sits on top of it
                    zIndex: 999,
                    position: 'fixed',
                    top: 0,
                    left: `calc(${position}vw - ${HIT_WIDTH / 2}px)`,
                    width: `${HIT_WIDTH}px`,
                    height: '100vh',
                    cursor: 'col-resize',
                    touchAction: 'none',
                    outline: 'none',
                    display: 'flex',
                    justifyContent: 'center',
                }}>
                <div style={{
                    width: active ? '4px' : '1px',
                    height: '100%',
                    background: active ? '#007fd4' : 'rgba(0, 0, 0, 0.12)',
                    transition: 'width 0.1s ease-out, background-color 0.1s ease-out',
                }} />
            </div>
            {dragging && (
                // While dragging, cover the whole page (including VSCode's iframes, which would otherwise
                // capture the pointer events), and keep the resize cursor and prevent text selection everywhere.
                <div style={{
                    zIndex: 1001,
                    position: 'fixed',
                    inset: 0,
                    cursor: 'col-resize',
                    userSelect: 'none',
                }} />
            )}
        </>
    );
};

export default SplitDivider;
