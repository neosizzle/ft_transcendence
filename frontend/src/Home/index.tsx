import React, { FunctionComponent } from "react";
import Footer from "./components/Footer";
import Hero from "./components/Hero";

const Home: FunctionComponent = () => {
  return (
    <div>
      {/* Hero section */}
      <Hero />

      {/* Footer */}
      <Footer />
    </div>
  );
};

export default Home;
