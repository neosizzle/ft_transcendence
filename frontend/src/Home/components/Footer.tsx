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
    name: "Edison Lim",
    intraName: "edlim",
    intraUrl: "https://profile.intra.42.fr/users/edlim",
    image: "/assets/default-pp.webp",
    github: "https://github.com/PandieYay",
  },
  {
    name: "Jun Han Ng",
    intraName: "jng",
    intraUrl: "https://profile.intra.42.fr/users/jng",
    image: "/assets/default-pp.webp",
    github: "https://github.com/neosizzle",
  },
  {
    name: "Wee Hean Ng",
    intraName: "weng",
    intraUrl: "https://profile.intra.42.fr/users/weng",
    image: "/assets/default-pp.webp",
    github: "https://github.com/nwhean",
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
