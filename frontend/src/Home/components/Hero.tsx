import React, { FunctionComponent } from "react";
import { useNavigate } from "react-router-dom";
import { useAuth } from "../../context/authContext";

const Hero: FunctionComponent = () => {
  const auth = useAuth();
  const navigate = useNavigate();

  return (
    <div className="min-h-screen bg-fixed bg-[url('/public/assets/home-bg.gif')] flex justify-center items-center">
      {/* Container */}
      <div>
        <div className="flex justify-center">
          <img
            src="/assets/logo/42logo-transparent.png"
            className="h-3/4 w-3/4 my-5"
          />
        </div>
        <div className="flex justify-center">
          <button
            onClick={() => {
              if (auth?.user) navigate("/game");
              else navigate("/login");
            }}
            className="block bg-black text-white text-xl px-8 py-4 rounded-lg my-5 hover:bg-slate-800 active:bg-white active:text-black"
          >
            {auth?.user ? "Play" : "Login"}
          </button>
        </div>
      </div>
    </div>
  );
};

export default Hero;
