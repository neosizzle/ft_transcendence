import { useState, useEffect } from "react";

const useWindowDimensions = () => {
  const hasWindow: boolean = typeof window !== "undefined";

  const getWindowDimensions = () => {
    const width: number | null = hasWindow ? window.innerWidth : null;
    const height: number | null = hasWindow ? window.innerHeight : null;
    return {
      width,
      height,
    };
  };

  const [windowDimensions, setWindowDimensions] = useState(
    getWindowDimensions()
  );

  const handleResize = () => {
    setWindowDimensions(getWindowDimensions());
  };

  useEffect(() => {
    if (hasWindow) {
      window.addEventListener("resize", handleResize);
      return () => window.removeEventListener("resize", handleResize);
    }
  }, [hasWindow]);

  return windowDimensions;
};

export default useWindowDimensions;
