import React, { FunctionComponent } from "react";
import AckCard from "./AckCard";

export interface MemberAck {
  name: string;
  intraName: string;
  intraUrl: string;
  image: string;
  github?: string;
  linkedin?: string;
}

const members: MemberAck[] = [
  {
    name: "Jun Han",
    intraName: "jng",
    intraUrl: "www.youtube.com",
    image: "/assets/default-pp.webp",
    github: "wewewewe",
    linkedin: "asdsadasd",
  },
  {
    name: "Jun Han",
    intraName: "jng",
    intraUrl: "www.youtube.com",
    image: "/assets/default-pp.webp",
  },
  {
    name: "Jun Han",
    intraName: "jng",
    intraUrl: "www.youtube.com",
    image: "/assets/default-pp.webp",
  },
];

const Footer: FunctionComponent = () => {
  return (
    <div className="min-h-screen bg-black text-white flex flex-col justify-center items-center">
      <div className="text-4xl lg:text-7xl font-semibold mt-20 mb-10">
        Acknowledgement
      </div>

      <div className="mt-10">
        {members.map((member, i) => (
          <AckCard index={i} total={members.length} member={member} key={i} />
        ))}
      </div>
    </div>
  );
};

export default Footer;
